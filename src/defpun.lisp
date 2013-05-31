;;; Copyright (c) 2011-2012, James M. Lawrence. All rights reserved.
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

(in-package #:lparallel.defpun)

(import-now alexandria:simple-style-warning
            lparallel.util::symbolicate/package
            lparallel.kernel::kernel
            lparallel.kernel::unwrap-result
            lparallel.kernel::wrap-error
            lparallel.kernel::make-task
            lparallel.kernel::make-task-fn
            lparallel.kernel::submit-raw-task
            lparallel.kernel::%kernel-worker-count
            lparallel.kernel::accept-task-p
            lparallel.kernel::limiter-data
            lparallel.kernel::with-limiter-slots
            lparallel.kernel::*make-limiter-data*
            lparallel.kernel::*worker*
            lparallel.kernel::steal-work
            lparallel.kernel::*kernel*
            lparallel.kernel::call-with-task-handler)

;;;; spin lock

#+lparallel.with-cas
(progn
  (defun make-spin-lock ()
    nil)

  (defmacro/once with-spin-lock-held (((access &once container)) &body body)
    `(locally (declare #.*full-optimize*)
       (unwind-protect/ext
        :prepare (loop :until (cas (,access ,container) nil t))
        :main (progn ,@body)
        :cleanup (setf (,access ,container) nil)))))

#-lparallel.with-cas
(progn
  (defun make-spin-lock ()
    (make-lock))

  (defmacro with-spin-lock-held (((access container)) &body body)
    `(with-lock-held ((,access ,container))
       ,@body)))

;;;; util

(defmacro with-lock-predicate/wait*
    (&key lock predicate1 predicate2 succeed/lock succeed/no-lock fail)
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

(defun has-lambda-list-keyword-p (list)
  (some (lambda (elem) (find elem lambda-list-keywords)) list))

(defmacro defun/wrapper (wrapper-name impl-name params &body body)
  (with-gensyms (args kernel)
    (multiple-value-bind (wrapper-params expansion)
        (if (has-lambda-list-keyword-p params)
            (values `(&rest ,args)
                    ``(apply (function ,',impl-name) ,,kernel ,',args))
            (values params
                    ``(,',impl-name ,,kernel ,@',params)))
      `(defun ,wrapper-name ,wrapper-params
         (macrolet ((call-impl (,kernel) ,expansion))
           ,@body)))))

;;;; lightweight futures

(defconstant +no-result+ 'no-result)

(defmacro result (future) `(car ,future))
(defmacro future-fn (future) `(cdr ,future))

(defun/inline %make-future (fn)
  (cons +no-result+ fn))

(defun make-future (kernel fn)
  (declare #.*full-optimize*)
  (let ((future (%make-future fn)))
    (submit-raw-task
     (make-task
      (lambda ()
        (unwind-protect/ext
         :main  (setf (result future) (call-with-task-handler
                                       (future-fn future)))
         :abort (setf (result future) (wrap-error 'task-killed-error)))))
     kernel)
    future))

(defmacro future (kernel &body body)
  `(make-future ,kernel (make-task-fn ,@body)))

(defun/inline fulfilledp (future)
  (declare #.*full-optimize*)
  (not (eq (result future) +no-result+)))

(defun/inline force (future)
  (declare #.*full-optimize*)
  (unwrap-result (result future)))

;;;; future-let

(defun pairp (form)
  (and (consp form) (eql (length form) 2)))

(defmacro with-parsed-let-args ((pairs non-pairs syms) bindings &body body)
  (check-type bindings symbol)
  `(let* ((,pairs     (remove-if-not #'pairp ,bindings))
          (,non-pairs (remove-if     #'pairp ,bindings))
          (,syms      (loop
                         :for (name nil) :in ,pairs
                         :collect (gensym (symbol-name name)))))
     ,@body))

(defmacro future-let (&key kernel future force bindings pre-body body)
  (with-parsed-body (nil declares body)
    (with-parsed-let-args (pairs non-pairs syms) bindings
      `(symbol-macrolet ,(loop
                            :for sym :in syms
                            :for (name nil) :in pairs
                            :collect `(,name (,force ,@(unsplice kernel) ,sym)))
         (let (,@(loop
                    :for sym :in syms
                    :for (nil form) :in pairs
                    :collect `(,sym (,future ,@(unsplice kernel) ,form)))
               ,@non-pairs)
           ,@declares
           ,@(unsplice pre-body)
           ,@body)))))

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
  (loop
     :for name :in *registered-names*
     :collect `(,name (&rest args)
                 `(,',(unchecked-name name) ,',kernel ,@args))))

(defmacro declaim-defpun (&rest names)
  "See `defpun'."
  ;; This is used outside of the defpun macro.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (with-lock-held (*registration-lock*)
       ,@(loop
            :for name :in names
            :collect `(register-name ',name)))))

(defun delete-registered-names (names)
  ;; This is used outside of the defpun macro.
  (with-lock-held (*registration-lock*)
    (setf *registered-names* (set-difference *registered-names* names))))

;;;; defpun

;;; Must use struct due to CAS.
(locally (declare #.*full-optimize*)
  (defstruct (limiter-data (:constructor %make-limiter-data))
    (lock (make-spin-lock))
    (count 0 :type fixnum)
    (limit (error "no limit") :type fixnum)))

(defun make-limiter-data (worker-count use-caller)
  (%make-limiter-data :limit (if use-caller
                                 (1+ worker-count)
                                 worker-count)))

(setf *make-limiter-data* 'make-limiter-data)

(defun/inline use-caller-p (kernel)
  (/= (limiter-data-limit (limiter-data kernel))
      (%kernel-worker-count kernel)))

(defun/type update-task-count/no-lock (kernel delta) (kernel fixnum) t
  (declare #.*full-optimize*)
  (with-limiter-slots (accept-task-p limiter-data) kernel
    (incf (limiter-data-count limiter-data) delta)
    ;; `<=' returns generalized boolean
    (setf accept-task-p
          (to-boolean (<= (limiter-data-count limiter-data)
                          (the fixnum (limiter-data-limit limiter-data)))))))

(defun/type update-task-count (kernel delta) (kernel fixnum) t
  (declare #.*full-optimize*)
  (with-spin-lock-held ((limiter-data-lock (limiter-data kernel)))
    (update-task-count/no-lock kernel delta)))

(defmacro future/fast (kernel &body body)
  `(future ,kernel
     (unwind-protect
          (progn ,@body)
       (update-task-count ,kernel -1))))

(defun force/fast (kernel future)
  (declare #.*full-optimize*)
  (if (fulfilledp future)
      (force future)
      (let ((worker *worker*))
        (loop
           (when (fulfilledp future)
             (return (force future)))
           #+lparallel.with-green-threads (thread-yield)
           (steal-work kernel worker)))))

(defmacro %%plet/fast (kernel predicate future-count bindings body)
  `(with-lock-predicate/wait*
       :lock            (limiter-data-lock (limiter-data ,kernel))
       :predicate1      ,predicate
       :predicate2      (accept-task-p ,kernel)
       :succeed/lock    (update-task-count/no-lock ,kernel ,future-count)
       :succeed/no-lock (future-let :kernel ,kernel
                                    :future future/fast
                                    :force force/fast
                                    :bindings ,bindings
                                    :body ,body)
       :fail            (let ,bindings ,@body)))

(defmacro %plet/fast (kernel predicate bindings body)
  (with-parsed-let-args (pairs non-pairs syms) bindings
    (declare (ignore non-pairs syms))
    (if pairs
        `(%%plet/fast ,kernel ,predicate ,(length pairs) ,bindings ,body)
        `(let ,bindings ,@body))))

(defmacro plet/fast (kernel bindings &body body)
  `(%plet/fast ,kernel
               (accept-task-p ,kernel)
               ,bindings
               ,body))

(defmacro plet-if/fast (kernel predicate bindings &body body)
  `(%plet/fast ,kernel
               (and (accept-task-p ,kernel) ,predicate)
               ,bindings ,body))

(defmacro/once call-impl-in-worker (&once kernel)
  (with-gensyms (worker channel)
    `(let ((,worker *worker*))
       (if ,worker
           (call-impl ,kernel)
           (let ((,channel (make-channel)))
             (submit-task ,channel (lambda ()
                                     (multiple-value-list (call-impl ,kernel))))
             (values-list (receive-result ,channel)))))))

(defmacro define-defpun (defpun doc defun &rest types)
  `(defmacro ,defpun (name params ,@types &body body)
     ,doc
     (with-parsed-body (docstring declares body)
       (with-lock-held (*registration-lock*)
         ;; these two calls may affect the registered macrolets in the
         ;; return form below
         (delete-stale-registrations)
         (register-name name)
         (with-gensyms (kernel)
           `(progn
              (,',defun ,(unchecked-name name) (,kernel ,@params)
                  ,,@(unsplice (when types ``(kernel ,@,(first types))))
                  ,,@(unsplice (when types (second types)))
                ,@declares
                (declare (ignorable ,kernel))
                (macrolet ((plet (bindings &body body)
                             `(plet/fast ,',kernel ,bindings
                                ,@body))
                           (plet-if (predicate bindings &body body)
                               `(plet-if/fast ,',kernel ,predicate ,bindings
                                  ,@body))
                           ,@(registered-macrolets kernel))
                  ,@body))
              (defun/wrapper ,name ,(unchecked-name name) ,params
                ,@(unsplice docstring)
                (let ((kernel (check-kernel)))
                  (if (use-caller-p kernel)
                      (call-impl kernel)
                      (call-impl-in-worker kernel))))
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
