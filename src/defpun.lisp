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
            lparallel.kernel::unwrap-result
            lparallel.kernel::wrap-error
            lparallel.kernel::make-task
            lparallel.kernel::make-task-fn
            lparallel.kernel::submit-raw-task
            lparallel.kernel::%kernel-worker-count
            lparallel.kernel::optimizer-data
            lparallel.kernel::optimizer-flag
            lparallel.kernel::with-optimizer-slots
            lparallel.kernel::*make-optimizer-data*
            lparallel.kernel::*worker*
            lparallel.kernel::steal-work
            lparallel.kernel::*kernel*
            lparallel.kernel::call-with-task-handler)

;;;; spin lock

#+sbcl
(defmacro cas (place old new)
  (check-type old symbol)
  `(eq ,old (sb-ext:compare-and-swap ,place ,old ,new)))

#+lispworks
(defmacro cas (place old new)
  `(sys:compare-and-swap ,place ,old ,new))

#+ccl
(defmacro cas (place old new)
  `(ccl::conditional-store ,place ,old ,new))

#+(or sbcl lispworks ccl)
(progn
  (defun make-spin-lock ()
    nil)

  (defmacro/once with-spin-lock-held (((access &once container)) &body body)
    `(locally (declare #.*full-optimize*)
       (unwind-protect/ext
        :prepare (loop :until (cas (,access ,container) nil t))
        :main (progn ,@body)
        :cleanup (setf (,access ,container) nil)))))

#-(or sbcl lispworks ccl)
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

;;; defpun relies upon the inlined optimizer-flag call in order to
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

(defun unchecked-name (name)
  ;; We could intern this into a private package and maintain an alist
  ;; of (public . private) package pairs, but that seems
  ;; over-engineered. Anonymous packages don't exist anyway.
  (symbolicate/package (symbol-package name) '#:%%.defpun. name) )

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
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop
          :for name :in names
          :collect `(register-name ',name))))

(defun delete-registered-names (names)
  (setf *registered-names* (set-difference *registered-names* names)))

;;;; defpun

(locally (declare #.*full-optimize*)
  (defstruct task-counter
    (count 0 :type fixnum)
    (lock (make-spin-lock))))

(setf *make-optimizer-data* 'make-task-counter)

(defmacro task-counter (kernel)
  `(optimizer-data ,kernel))

(defmacro/once with-task-counter-slots (slots &once task-counter &body body)
  (assert (equal '(count) slots))
  `(symbol-macrolet ((count (task-counter-count ,task-counter)))
     ,@body))

(defmacro accept-task-p (kernel)
  (check-type kernel symbol)
  ;; needs fast inline for small functions
  `(locally (declare #.*full-optimize*)
     (the boolean (optimizer-flag (the kernel ,kernel)))))

(defmacro define-update-task-count/no-lock (name op)
  `(defun/type ,name (kernel delta) (kernel fixnum) t
     (declare #.*full-optimize*)
     (with-optimizer-slots ((accept-task-p optimizer-flag)
                            (task-counter optimizer-data)) kernel
       (with-task-counter-slots (count) task-counter
         (incf count delta)
         ;; `<=' returns generalized boolean
         (setf accept-task-p
               (to-boolean (<= count (the fixnum (,op kernel)))))))))

(macrolet ((n-workers   (k) `(%kernel-worker-count ,k))
           (n-1-workers (k) `(1+ (%kernel-worker-count ,k))))
  (define-update-task-count/no-lock update-task-count/no-lock    n-workers)
  (define-update-task-count/no-lock update-task-count/no-lock/-1 n-1-workers))

(defmacro define-update-task-count (name op)
  `(defun/type ,name (kernel delta) (kernel fixnum) t
     (declare #.*full-optimize*)
     (with-spin-lock-held ((task-counter-lock (task-counter kernel)))
       (,op kernel delta))))

(define-update-task-count update-task-count    update-task-count/no-lock)
(define-update-task-count update-task-count/-1 update-task-count/no-lock/-1)

(defmacro define-future/fast (name update-task-count)
  `(defmacro ,name (kernel &body body)
     `(future ,kernel
        (unwind-protect
             (progn ,@body)
          (,',update-task-count ,kernel -1)))))

(define-future/fast future/fast    update-task-count)
(define-future/fast future/fast/-1 update-task-count/-1)

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

(defmacro %%plet/fast (kernel future/fast update-task-count/no-lock
                       predicate future-count bindings body)
  `(with-lock-predicate/wait*
       :lock            (task-counter-lock (task-counter ,kernel))
       :predicate1      ,predicate
       :predicate2      (accept-task-p ,kernel)
       :succeed/lock    (,update-task-count/no-lock ,kernel ,future-count)
       :succeed/no-lock (future-let :kernel ,kernel
                                    :future ,future/fast
                                    :force force/fast
                                    :bindings ,bindings
                                    :body ,body)
       :fail            (let ,bindings ,@body)))

(defmacro %plet/fast (kernel future/fast update-task-count/no-lock
                      predicate bindings body)
  (with-parsed-let-args (pairs non-pairs syms) bindings
    (declare (ignore non-pairs syms))
    (if pairs
        `(%%plet/fast ,kernel ,future/fast ,update-task-count/no-lock
                      ,predicate ,(length pairs) ,bindings ,body)
        `(let ,bindings ,@body))))

(defmacro define-plet/fast (name
                            future/fast
                            update-task-count/no-lock)
  `(defmacro ,name (kernel bindings &body body)
     `(%plet/fast ,kernel
                  ,',future/fast
                  ,',update-task-count/no-lock
                  (accept-task-p ,kernel)
                  ,bindings
                  ,body)))

(define-plet/fast plet/fast
                  future/fast
                  update-task-count/no-lock)

(define-plet/fast plet/fast/-1
                  future/fast/-1
                  update-task-count/no-lock/-1)

(defmacro define-plet-if/fast (name
                               future/fast
                               update-task-count/no-lock)
  `(defmacro ,name (kernel predicate bindings &body body)
     `(%plet/fast ,kernel
                  ,',future/fast
                  ,',update-task-count/no-lock
                  (and (accept-task-p ,kernel) ,predicate)
                  ,bindings ,body)))

(define-plet-if/fast plet-if/fast
                     future/fast
                     update-task-count/no-lock)

(define-plet-if/fast plet-if/fast/-1
                     future/fast/-1
                     update-task-count/no-lock/-1)

(defmacro define-defpun (defpun doc defun plet/fast plet-if/fast call-impl
                         &rest types)
  `(defmacro ,defpun (name params ,@types &body body)
     ,doc
     (with-parsed-body (docstring declares body)
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
                           `(,',',plet/fast ,',kernel ,bindings
                              ,@body))
                         (plet-if (predicate bindings &body body)
                           `(,',',plet-if/fast ,',kernel ,predicate ,bindings
                              ,@body))
                         ,@(registered-macrolets kernel))
                ,@body))
            (defun/wrapper ,name ,(unchecked-name name) ,params
              ,@(unsplice docstring)
              (,',call-impl (check-kernel)))
            (eval-when (:load-toplevel :execute)
              (register-fn ',name))
            ',name)))))

(defmacro/once call-impl-in-worker (&once kernel)
  (with-gensyms (worker channel)
    `(let ((,worker *worker*))
       (if ,worker
           (call-impl ,kernel)
           (let ((,channel (make-channel)))
             (submit-task ,channel (lambda ()
                                     (multiple-value-list (call-impl ,kernel))))
             (values-list (receive-result ,channel)))))))

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
  defun
  plet/fast
  plet-if/fast
  call-impl-in-worker)

(define-defpun defpun/type
  "Typed version of `defpun'.

`arg-types' is an unevaluated list of argument types.

`return-type' is an unevaluated form of the return type, possibly
indicating multiple values as in (values fixnum float).

\(As a technical point, if `return-type' contains no lambda list
keywords then the return type given to ftype will be additionally
constrained to match the number of return values specified.)"
  defun/type
  plet/fast
  plet-if/fast
  call-impl-in-worker
  arg-types
  return-type)

(define-defpun defpun*
  "Like `defpun' except that it defines a function which is optimized
for N-1 worker threads where N is the number of cores.

A function defined with `defpun*' differs from its unstarred
counterpart in two ways: it has less overhead, and the thread which
calls it always participates in the computation.

In contrast, the caller of a function defined by `defpun' participates
in the computation only when the caller is in a worker thread."
  defun plet/fast/-1
  plet-if/fast/-1
  call-impl)

(define-defpun defpun/type*
  "Typed version of `defpun*'. Also see `defpun/type'."
  defun/type
  plet/fast/-1
  plet-if/fast/-1
  call-impl
  arg-types
  return-type)
