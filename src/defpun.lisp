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

(import-now lparallel.util::symbolicate/package
            lparallel.kernel::kernel
            lparallel.kernel::%kernel-worker-count
            lparallel.kernel::optimizer-data
            lparallel.kernel::optimizer-flag
            lparallel.kernel::with-optimizer-slots
            lparallel.kernel::*make-optimizer-data*
            lparallel.kernel::*worker*
            lparallel.kernel::steal-work
            lparallel.promise::%future
            lparallel.promise::fulfilledp/promise)

;;;; util

(defmacro with-lock-predicate/wait*
    (&key lock predicate1 predicate2 succeed/lock succeed/no-lock fail)
  (with-gensyms (top fail-tag)
    `(block ,top
       (tagbody
          (when ,predicate1
            (with-lock-held (,lock)
              (if ,predicate2
                  ,succeed/lock
                  (go ,fail-tag)))
            (return-from ,top ,succeed/no-lock))
        ,fail-tag
          (return-from ,top ,fail)))))

(defun/type/inline negate (x) (fixnum) fixnum
  (- 0 x))

(defun has-lambda-list-keyword-p (list)
  (some (lambda (elem) (find elem lambda-list-keywords)) list))

(defmacro defun/wrapper (wrapper-name impl-name params &body body)
  (with-gensyms (args)
    (multiple-value-bind (wrapper-params call-impl)
        (if (has-lambda-list-keyword-p params)
            (values `(&rest ,args)
                    `(apply (function ,impl-name) ,args))
            (values params
                    `(,impl-name ,@params)))
      `(defun ,wrapper-name ,wrapper-params
         (macrolet ((call-impl () ',call-impl))
           ,@body)))))

;;;; future-let

(defun pairp (form)
  (and (consp form) (eql (length form) 2)))

(defmacro/once with-parsed-let-args ((pairs non-pairs syms)
                                     &once bindings
                                     &body body)
  `(let* ((,pairs     (remove-if-not #'pairp ,bindings))
          (,non-pairs (remove-if     #'pairp ,bindings))
          (,syms      (loop
                         :for (name nil) :in ,pairs
                         :collect (gensym (symbol-name name)))))
     ,@body))

(defmacro future-let (&key future force bindings pre-body body)
  (with-parsed-body (nil declares body)
    (with-parsed-let-args (pairs non-pairs syms) bindings
      `(symbol-macrolet ,(loop
                            :for sym :in syms
                            :for (name nil) :in pairs
                            :collect `(,name (,force ,sym)))
         (let (,@(loop
                    :for sym :in syms
                    :for (nil form) :in pairs
                    :collect `(,sym (,future ,form)))
               ,@non-pairs)
           ,@declares
           ,@(unsplice pre-body)
           ,@body)))))

;;;; function registration

;;; defpun relies upon the inlined optimizer-flag call in order to
;;; achieve speedup, which means that *kernel* must exist before the
;;; call takes place. If *kernel* is nil, the error may be confusing
;;; due to inlining and optimizations. Inserting `check-kernel' into
;;; the body of defpun functions negates the speedup for small
;;; functions.

;;; Thus for user-friendliness we define checked and unchecked
;;; functions for each defpun form. The user calls the checked
;;; version; defpun calls the unchecked one via a macrolet.

;;; The macrolets also have the happy side-effect of preventing
;;; reference to the checked (slower) function via #'.

(defvar *registered-fns* nil)

(defun unchecked-name (name)
  ;; We could intern this into a private package and maintain an alist
  ;; of (public . private) package pairs, but that seems
  ;; over-engineered. Anonymous packages don't exist anyway.
  (symbolicate/package (symbol-package name) '#:%%.defpun. name) )

(defun defpun-loaded-p (name)
  (ignore-errors
    (and (symbol-function name)
         (symbol-function (unchecked-name name)))))

(defun register-fn-name (name)
  (pushnew name *registered-fns*))

(defun register-fn (name)
  (register-fn-name name)
  (when (defpun-loaded-p name)
    (setf (get name 'checked-fn) (symbol-function name))
    (setf (get name 'unchecked-fn) (symbol-function (unchecked-name name)))))

(defun valid-registered-fn-p (name)
  ;; not uninterned and not replaced with regular defun
  (and (symbol-package name)
       (if (get name 'checked-fn)
           (and (eq (symbol-function name)
                    (get name 'checked-fn))
                (eq (symbol-function (unchecked-name name))
                    (get name 'unchecked-fn)))
           ;; seen but not compiled yet
           t)))

(defun validate-registered-fns ()
  (setf *registered-fns* (delete-if-not #'valid-registered-fn-p
                                        *registered-fns*)))

(defun registered-macrolets ()
  (loop
     :for name :in *registered-fns*
     :collect `(,name (&rest args) `(,',(unchecked-name name) ,@args))))

(defmacro declaim-defpun (&rest names)
  "See `defpun'."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop
          :for name :in names
          :collect `(register-fn-name ',name))))

;;;; defpun

(locally (declare #.*full-optimize*)
  (defslots task-counter ()
    ((count              :initform 0 :type fixnum)
     (lock  :reader lock :initform (make-lock)))))

(setf *make-optimizer-data* 'make-task-counter-instance)

(defmacro accept-task-p ()
  ;; needs fast inline for small functions
  `(locally (declare #.*full-optimize*)
     (the boolean (optimizer-flag (the kernel *kernel*)))))

(defun/type update-task-count/no-lock (kernel delta) (kernel fixnum) t
  (declare #.*full-optimize*)
  (with-optimizer-slots (optimizer-data optimizer-flag) kernel
    (with-task-counter-slots (count) optimizer-data
      (incf count delta)
      ;; `<=' returns generalized boolean
      (setf optimizer-flag (to-boolean
                            (<= count
                                (the fixnum
                                  (1+ (%kernel-worker-count kernel)))))))))

(defun/type update-task-count (kernel delta) (kernel fixnum) t
  (declare #.*normal-optimize*)
  (with-lock-held ((lock (optimizer-data kernel)))
    (update-task-count/no-lock kernel delta)))

(defmacro future/fast (&body body)
  (with-gensyms (kernel)
    `(let1 ,kernel (the kernel *kernel*)
       (declare (type kernel ,kernel))
       (future
         (unwind-protect
              (progn ,@body)
           (update-task-count ,kernel -1))))))

(defun/type force/fast (future) (%future) t
  (declare #.*normal-optimize*)
  (let1 kernel *kernel*
    (loop
       (when (fulfilledp/promise future)
         (return (force future)))
       (steal-work kernel *worker*))))

(defmacro %%plet/fast (predicate future-count bindings body)
  (with-gensyms (all-created-p)
    `(with-lock-predicate/wait*
         :lock            (lock (optimizer-data *kernel*))
         :predicate1      ,predicate
         :predicate2      (accept-task-p)
         :succeed/lock    (update-task-count/no-lock *kernel* ,future-count)
         :succeed/no-lock (let1 ,all-created-p nil
                            (unwind-protect
                                 (future-let :future future/fast
                                             :force force/fast
                                             :bindings ,bindings
                                             :pre-body (setf ,all-created-p t)
                                             :body ,body)
                              ;; In the rare event of future
                              ;; creation failure, roll back the
                              ;; optimizer count.
                              ;; 
                              ;; It's OK if some futures were
                              ;; created; defpun will just
                              ;; temporarily become more accepting
                              ;; of tasks.
                              (when (not ,all-created-p)
                                (update-task-count *kernel*
                                                   (negate ,future-count)))))
         :fail            (let ,bindings ,@body))))

(defmacro %plet/fast (predicate bindings body)
  (with-parsed-let-args (pairs non-pairs syms) bindings
    (declare (ignore non-pairs syms))
    (if pairs
        `(%%plet/fast ,predicate ,(length pairs) ,bindings ,body)
        `(let ,bindings ,@body))))

(defmacro plet/fast (bindings &body body)
  `(%plet/fast (accept-task-p) ,bindings ,body))

(defmacro plet-if/fast (predicate bindings &body body)
  `(%plet/fast (and (accept-task-p) ,predicate)
               ,bindings
               ,body))

(defmacro define-defpun (defpun defun doc &rest types)
  `(defmacro ,defpun (name params ,@types &body body)
     ,doc
     (with-parsed-body (docstring declares body)
       (validate-registered-fns)
       (register-fn-name name)
       `(progn
          (,',defun ,(unchecked-name name) ,params ,,@types
            ,@declares
            (macrolet ((plet (bindings &body body)
                         `(plet/fast ,bindings ,@body))
                       (plet-if (predicate bindings &body body)
                         `(plet-if/fast ,predicate ,bindings ,@body))
                       ,@(registered-macrolets))
              ,@body))
          (defun/wrapper ,name ,(unchecked-name name) ,params
            ,@(unsplice docstring)
            (check-kernel)
            (call-impl))
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (register-fn ',name))))))

(define-defpun defpun defun
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
")

(define-defpun defpun/type defun/type
  "Typed version of `defpun'. 

`arg-types' is an unevaluated list of argument types.

`return-type' is an unevaluated form of the return type, possibly
indicating multiple values as in (values fixnum float).

\(As a technical point, if `return-type' contains no lambda list
keywords then the return type given to ftype will be additionally
constrained to match the number of return values specified.)"
  arg-types return-type)
