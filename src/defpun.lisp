;;; Copyright (c) 2014, James M. Lawrence. All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials provided
;;;       with the distribution.
;;;
;;;     * Neither the name of the project nor the names of its
;;;       contributors may be used to endorse or promote products derived
;;;       from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage #:lparallel.defpun
  (:documentation "Fine-grained parallelism.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.kernel
        #:lparallel.thread-util
        #:lparallel.slet)
  (:export #:defpun
           #:defpun*
           #:defpun/type
           #:defpun/type*
           #:declaim-defpun
           #:plet
           #:plet-if
           #:slet)
  (:import-from #:alexandria
                #:simple-style-warning)
  (:import-from #:lparallel.util
                #:symbolicate/package)
  (:import-from #:lparallel.slet
                #:make-binding-data
                #:parse-bindings)
  (:import-from #:lparallel.kernel
                #:*worker*
                #:*make-limiter-data*
                #:kernel
                #:use-caller-p
                #:unwrap-result
                #:call-with-task-handler
                #:limiter-accept-task-p
                #:limiter-count
                #:limiter-lock
                #:submit-raw-task
                #:make-task
                #:task-lambda
                #:wrapped-error
                #:with-task-context
                #:steal-work))

(in-package #:lparallel.defpun)

;;;; function registration

;;; defpun relies upon the inlined accept-task-p call in order to
;;; achieve speedup, which means that *kernel* must exist before the
;;; call takes place. If *kernel* is nil, the error may be confusing
;;; due to inlining and optimizations. Inserting `check-kernel' into
;;; the body of defpun functions negates the speedup for small
;;; functions.
;;;
;;; Thus for user-friendliness we define checked and unchecked
;;; functions for each defpun form. The user calls the checked
;;; version; defpun calls the unchecked one via a macrolet.
;;;
;;; The macrolets also have the happy side-effect of preventing
;;; reference to the checked (slower) function via #'.
;;;
;;; We store references to the checked and unchecked functions in
;;; order to detect redefinitions with defun or otherwise.

(defconstant +checked-key+ 'checked-key)
(defconstant +unchecked-key+ 'unchecked-key)

(defvar *registered-names* nil)
(defvar *registration-lock* (make-lock))

(defun unchecked-name (name)
  ;; We could intern this into a private package and maintain an alist
  ;; of (public . private) package pairs, but that seems
  ;; over-engineered. Anonymous packages don't exist anyway.
  (symbolicate/package (symbol-package name) '#:%%%%.defpun. name) )

(defun register-name (name)
  (pushnew name *registered-names*))

(defun register-fn (name)
  (setf (get name +checked-key+) (symbol-function name))
  (setf (get name +unchecked-key+) (symbol-function (unchecked-name name))))

(defun registered-fn-p (name)
  (get name +checked-key+))

(defun valid-registered-fn-p (name)
  (and (fboundp name)
       (eq (symbol-function name)
           (get name +checked-key+))
       (fboundp (unchecked-name name))
       (eq (symbol-function (unchecked-name name))
           (get name +unchecked-key+))))

;;; a name may be registered without having a corresponding function
(defun valid-registered-name-p (name)
  (and (symbol-package name)
       (or (not (registered-fn-p name))
           (valid-registered-fn-p name))))

(defun delete-stale-registrations ()
  (setf *registered-names*
        (remove-if-not #'valid-registered-name-p *registered-names*)))

(defun registered-macrolets (kernel)
  (loop for name in *registered-names*
        collect `(,name (&rest args)
                   `(,',(unchecked-name name) ,',kernel ,@args))))

(defmacro declaim-defpun (&rest names)
  "See `defpun'."
  ;; This is used outside of the defpun macro.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (with-lock-held (*registration-lock*)
       ,@(loop for name in names
               collect `(register-name ',name)))))

(defun delete-registered-names (names)
  ;; This is used outside of the defpun macro.
  (with-lock-held (*registration-lock*)
    (setf *registered-names* (set-difference *registered-names* names))))

;;;; limiter

;;; New tasks are accepted when limiter-count is positive. Creating a
;;; task decrements limiter-count; finishing a task increments it.

(defun initial-limiter-count (thread-count)
  (+ thread-count 1))

(defun make-limiter-data (thread-count)
  (list :limiter-accept-task-p t
        :limiter-count (initial-limiter-count thread-count)
        :limiter-lock (make-spin-lock)))

(setf *make-limiter-data* 'make-limiter-data)

(defmacro accept-task-p (kernel)
  (check-type kernel symbol)
  `(locally (declare #.*full-optimize*)
     (limiter-accept-task-p (the kernel ,kernel))))

(defun/type update-limiter-count/no-lock (kernel delta) (kernel fixnum) (values)
  (declare #.*full-optimize*)
  (incf (limiter-count kernel) delta)
  (setf (limiter-accept-task-p kernel)
        (to-boolean (plusp (the fixnum (limiter-count kernel)))))
  (values))

(defun/type update-limiter-count (kernel delta) (kernel fixnum) (values)
  (declare #.*full-optimize*)
  (with-spin-lock-held ((limiter-lock kernel))
    (update-limiter-count/no-lock kernel delta))
  (values))

;;;; plet

(defconstant +no-result+ 'no-result)

(defmacro msetq (vars form)
  (if (= 1 (length vars))
      `(setq ,(first vars) ,form)
      `(multiple-value-setq ,vars ,form)))

(defun client-vars (binding-data)
  (reduce #'append binding-data :key #'first))

(defun temp-vars (binding-data)
  (reduce #'append binding-data :key #'second))

(defun primary-temp-vars (binding-data)
  (loop for (nil temp-vars nil) in binding-data
        collect (first temp-vars)))

(defmacro with-temp-bindings (here-binding-datum spawn-binding-data &body body)
  `(let (,@(temp-vars (list here-binding-datum))
         ,@(loop for var in (temp-vars spawn-binding-data)
                 collect `(,var +no-result+)))
     ,@body))

(defmacro with-client-bindings (binding-data null-bindings &body body)
  `(let (,@null-bindings
         ,@(mapcar #'list
                   (client-vars binding-data)
                   (temp-vars binding-data)))
     ,@body))

(defmacro spawn (kernel temp-vars form)
  (check-type kernel symbol)
  `(submit-raw-task
    (make-task
     (task-lambda
       ;; task handler already established
       (unwind-protect (msetq ,temp-vars (with-task-context ,form))
         (locally (declare #.*full-optimize*)
           (update-limiter-count (the kernel ,kernel) 1)))
       (values)))
    ,kernel))

(defmacro spawn-tasks (kernel spawn-binding-data)
  (check-type kernel symbol)
  `(progn
     ,@(loop for (nil temp-vars form) in spawn-binding-data
             collect `(spawn ,kernel ,temp-vars ,form))))

(defmacro exec-task (here-binding-datum)
  (destructuring-bind (client-vars temp-vars form) here-binding-datum
    (declare (ignore client-vars))
    `(msetq ,temp-vars ,form)))

(defmacro sync (kernel spawn-binding-data)
  (check-type kernel symbol)
  ;; reverse to check last spawn first
  (let ((temp-vars (reverse (temp-vars spawn-binding-data))))
    `(locally (declare #.*full-optimize*)
       (loop with worker = *worker*
             while (or ,@(loop for temp-var in temp-vars
                               collect `(eq ,temp-var +no-result+)))
             do #+lparallel.with-green-threads (thread-yield)
                (steal-work (the kernel ,kernel) worker)))))

(defmacro scan-for-errors (binding-data)
  ;; a wrapped error would only appear as the primary return value
  `(locally (declare #.*full-optimize*)
     ,@(loop for temp-var in (primary-temp-vars binding-data)
             collect `(when (typep ,temp-var 'wrapped-error)
                        (unwrap-result ,temp-var)))))

(defmacro %%%%plet (kernel bindings body)
  (multiple-value-bind (binding-data null-bindings) (make-binding-data bindings)
    (destructuring-bind
          (here-binding-datum &rest spawn-binding-data) binding-data
      `(with-temp-bindings ,here-binding-datum ,spawn-binding-data
         (spawn-tasks ,kernel ,spawn-binding-data)
         (exec-task ,here-binding-datum)
         (sync ,kernel ,spawn-binding-data)
         (scan-for-errors ,spawn-binding-data)
         (with-client-bindings ,binding-data ,null-bindings
           ,@body)))))

(defmacro with-lock-predicates (&key lock predicate1 predicate2
                                succeed/lock succeed/no-lock fail)
  (with-gensyms (top fail-tag)
    `(block ,top
       (tagbody
          (when ,predicate1
            (with-spin-lock-held (,lock)
              (if ,predicate2
                  ,succeed/lock
                  (go ,fail-tag)))
            (return-from ,top ,succeed/no-lock))
        ,fail-tag
          (return-from ,top ,fail)))))

(defmacro %%%plet (kernel predicate spawn-count bindings body)
  ;; Putting the body code into a shared dynamic-extent function
  ;; caused some slowdown, so reluctantly duplicate the body.
  `(with-lock-predicates
       :lock            (limiter-lock (the kernel ,kernel))
       :predicate1      ,predicate
       :predicate2      (accept-task-p ,kernel)
       :succeed/lock    (update-limiter-count/no-lock ,kernel ,(- spawn-count))
       :succeed/no-lock (%%%%plet ,kernel ,bindings ,body)
       :fail            (slet ,bindings ,@body)))

(defmacro %%plet (kernel predicate bindings body)
  (let ((spawn-count (- (length (parse-bindings bindings)) 1)))
    (if (plusp spawn-count)
        `(%%%plet ,kernel ,predicate ,spawn-count ,bindings ,body)
        `(slet ,bindings ,@body))))

(defmacro %plet (kernel bindings &body body)
  `(%%plet ,kernel
           (accept-task-p ,kernel)
           ,bindings
           ,body))

(defmacro %plet-if (kernel predicate bindings &body body)
  `(%%plet ,kernel
           (and (accept-task-p ,kernel) ,predicate)
           ,bindings
           ,body))

;;;; defpun

(defmacro defun/wrapper (wrapper-name impl-name lambda-list &body body)
  (with-gensyms (args kernel)
    (multiple-value-bind (wrapper-lambda-list expansion)
        (if (intersection lambda-list lambda-list-keywords)
            (values `(&rest ,args)
                    ``(apply (function ,',impl-name) ,,kernel ,',args))
            (values lambda-list
                    ``(,',impl-name ,,kernel ,@',lambda-list)))
      `(defun ,wrapper-name ,wrapper-lambda-list
         (macrolet ((call-impl (,kernel) ,expansion))
           ,@body)))))

(defun call-with-toplevel-handler (fn)
  (declare #.*full-optimize*)
  (declare (type function fn))
  (let* ((results (multiple-value-list (call-with-task-handler fn)))
         (first (first results)))
    (when (typep first 'wrapped-error)
      (unwrap-result first))
    (values-list results)))

(defun call-inside-worker (kernel fn)
  (declare #.*full-optimize*)
  (declare (type function fn))
  (let ((channel (let ((*kernel* kernel)) (make-channel))))
    (submit-task channel (lambda () (multiple-value-list (funcall fn))))
    (values-list (receive-result channel))))

(defun call-impl-fn (kernel impl)
  (declare #.*full-optimize*)
  (declare (type function impl))
  (if (or *worker* (use-caller-p kernel))
      (call-with-toplevel-handler impl)
      (call-inside-worker kernel impl)))

(defmacro define-defpun (defpun doc defun &rest types)
  `(defmacro ,defpun (name lambda-list ,@types &body body)
     ,doc
     (with-parsed-body (body declares docstring)
       (with-lock-held (*registration-lock*)
         ;; these two calls may affect the registered macrolets in the
         ;; return form below
         (delete-stale-registrations)
         (register-name name)
         (with-gensyms (kernel)
           `(progn
              (,',defun ,(unchecked-name name) (,kernel ,@lambda-list)
                  ,,@(unsplice (when types ``(kernel ,@,(first types))))
                  ,,@(unsplice (when types (second types)))
                ,@declares
                (declare (ignorable ,kernel))
                (macrolet ((plet (bindings &body body)
                             `(%plet ,',kernel ,bindings ,@body))
                           (plet-if (predicate bindings &body body)
                             `(%plet-if ,',kernel ,predicate ,bindings ,@body))
                           ,@(registered-macrolets kernel))
                  ,@body))
              (defun/wrapper ,name ,(unchecked-name name) ,lambda-list
                ,@(unsplice docstring)
                (let ((,kernel (check-kernel)))
                  (call-impl-fn ,kernel (lambda () (call-impl ,kernel)))))
              (eval-when (:load-toplevel :execute)
                (with-lock-held (*registration-lock*)
                  (register-fn ',name)))
              ',name))))))

(define-defpun defpun
  "`defpun' defines a function which is specially geared for
fine-grained parallelism. If you have many small tasks which bog down
the system, `defpun' may help.

The syntax of `defpun' matches that of `defun'. The difference is that
`plet' and `plet-if' take on new meaning inside `defpun'. The symbols
in the binding positions of `plet' and `plet-if' should be viewed as
lazily evaluated immutable references.

Inside a `defpun' form the name of the function being defined is a
macrolet, as are the names of other functions which were defined by
`defpun'. Thus using #' on them is an error. Calls to functions
defined by `defpun' entail more overhead when the caller lies outside
a `defpun' form.

A `defpun' function must exist before it is referenced inside another
`defpun' function. If this is not possible--for example if func1 and
func2 reference each other--then use `declaim-defpun' to specify
intent:

    (declaim-defpun func1 func2)
"
  defun)

(define-defpun defpun/type
  "Typed version of `defpun'.

`arg-types' is an unevaluated list of argument types.

`return-type' is an unevaluated form of the return type, possibly
indicating multiple values as in (values fixnum float).

\(As a technical point, if `return-type' contains no lambda list
keywords then the return type given to ftype will be additionally
constrained to match the number of return values specified.)"
  defun/type
  arg-types
  return-type)

(defmacro defpun* (&rest args)
  "Deprecated. Instead use `defpun' and pass `:use-caller t' to `make-kernel'."
  (simple-style-warning
   "`defpun*' is deprecated. Instead use `defpun' and pass ~
    `:use-caller t' to `make-kernel'.")
  `(defpun ,@args))

(defmacro defpun/type* (&rest args)
  "Deprecated. Instead use `defpun/type' and pass `:use-caller t' to
`make-kernel'."
  (simple-style-warning
   "`defpun/type*' is deprecated. Instead use `defpun/type' and pass ~
    `:use-caller t' to `make-kernel'.")
  `(defpun/type ,@args))
