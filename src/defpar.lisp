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

(in-package #:lparallel.defpar)

#.(import '(lparallel.util::conc-syms
            lparallel.kernel::kernel
            lparallel.kernel::%kernel-worker-count
            lparallel.kernel::make-optimizer-data
            lparallel.kernel::optimizer-data
            lparallel.kernel::optimizer-flag
            lparallel.kernel::with-optimizer-slots
            lparallel.kernel::*optimizer*
            lparallel.kernel::steal-work
            lparallel.cognate::with-parsed-let-args
            lparallel.cognate::future-let))

(alias-function accept-task-p optimizer-flag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; util
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun/inline negate (x)
  (declare (fixnum x))
  (the fixnum (- 0 (the fixnum x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; function registration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; defpar relies upon the inlined optimizer-flag call in order to
;;; achieve speedup, which means that *kernel* must exist before the
;;; call takes place. If *kernel* is nil, the error may be confusing
;;; due to inlining and optimizations. Inserting `check-kernel' into
;;; the body of defpar functions negates the speedup for small
;;; functions.

;;; Thus for user-friendliness we define checked and unchecked
;;; functions for each defpar form. The user calls the checked
;;; version; defpar calls the unchecked one via a macrolet.

;;; The macrolets also have the happy side-effect of preventing
;;; reference to the checked (slower) function via #'.

(defvar *registered-fns* nil)

(defun unchecked-name (name)
  (intern (conc-syms '#:%%%% name '#:/unchecked) (symbol-package name)))

(defun defpar-loaded-p (name)
  (ignore-errors
    (and (symbol-function name)
         (symbol-function (unchecked-name name)))))

(defun register-fn-name (name)
  (pushnew name *registered-fns*))

(defun register-fn (name)
  (register-fn-name name)
  (when (defpar-loaded-p name)
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

(defmacro declaim-defpar (&rest names)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop
          :for name :in names
          :collect `(register-fn-name ',name))))

(defun/inline to-boolean (x) (the boolean (if x t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; defpar
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *optimizer* 'defpar)

(defslots task-counter ()
  ((task-count              :initform 0 :type fixnum)
   (lock       :reader lock :initform (make-lock))))

(defmethod make-optimizer-data ((specializer (eql 'defpar)))
  (declare (ignore specializer))
  (make-task-counter-instance))

(defun/type update-task-count/no-lock (kernel delta) (kernel fixnum) t
  (declare #.*normal-optimize*)
  (with-optimizer-slots (optimizer-data optimizer-flag) kernel
    (with-task-counter-slots (task-count) optimizer-data
      (incf task-count delta)
      ;; optimizer-flag == accept-task-p
      ;; `<' returns generalized boolean
      (setf optimizer-flag (to-boolean
                            (<= task-count (%kernel-worker-count kernel)))))))

(defun/type update-task-count (kernel delta) (kernel fixnum) t
  (declare #.*normal-optimize*)
  (with-optimizer-slots (optimizer-data) kernel
    (with-task-counter-slots (lock) optimizer-data
      (with-lock-held (lock)
        (update-task-count/no-lock kernel delta)))))

(defmacro future/defpar (&body body)
  (with-gensyms (kernel)
    `(let1 ,kernel *kernel*
       (future
         (unwind-protect
              (progn ,@body)
           (update-task-count ,kernel -1))))))

(defmacro/once force/defpar (&once future)
  `(loop
      (when (fulfilledp ,future)
        (return (force ,future)))
      (steal-work)))

(defmacro %%plet/defpar (predicate pairs bindings body)
  (let1 count (length pairs)
    (with-gensyms (all-created-p)
      `(with-lock-predicate/wait*
           :lock            (lock (optimizer-data *kernel*))
           :predicate1      ,predicate
           :predicate2      (the boolean (accept-task-p *kernel*))
           :succeed/lock    (update-task-count/no-lock *kernel* ,count)
           :succeed/no-lock (let1 ,all-created-p nil
                              (unwind-protect
                                   (future-let :future future/defpar
                                               :force force/defpar
                                               :bindings ,bindings
                                               :pre-body (setf ,all-created-p t)
                                               :body ,body)
                                ;; In the rare event of future
                                ;; creation failure, roll back the
                                ;; optimizer count.
                                ;; 
                                ;; It's OK if some futures were
                                ;; created; defpar will just
                                ;; temporarily become more accepting
                                ;; of tasks.
                                (when (not ,all-created-p)
                                  (update-task-count *kernel*
                                                     (negate ,count)))))
           :fail            (let ,bindings ,@body)))))

(defmacro %plet/defpar (predicate bindings body)
  (with-parsed-let-args (pairs non-pairs syms) bindings
    (declare (ignore non-pairs syms))
    (if pairs
        `(%%plet/defpar ,predicate ,pairs ,bindings ,body)
        `(let ,bindings ,@body))))

(defmacro plet/defpar (bindings &body body)
  `(%plet/defpar (the boolean (accept-task-p *kernel*)) ,bindings ,body))

(defmacro plet-if/defpar (predicate bindings &body body)
  `(%plet/defpar (and (the boolean (accept-task-p *kernel*)) ,predicate)
                 ,bindings
                 ,body))

(defmacro defpar (name params &body body)
  "`defpar' is suitable for expressing fine-grained parallelism. If
you have many small tasks which bog down the system, `defpar' may
help.

The syntax of `defpar' matches that of `defun'. The difference is that
`plet' and `pfuncall' have a new meaning inside `defpar'. They may or
may not actually spawn parallel tasks, as determined by a run-time
optimizer.

The outcome is that speedup may be achieved with even small functions
like Fibonacci,

    (defpar fib (n)
      (declare (optimize (speed 3)))
      (if (< n 2)
          n
          (plet ((a (fib (- n 1)))
                 (b (fib (- n 2))))
            (+ a b))))

NOTE: `defpar' may require a high optimization level in order to
produce significant speedup."
  (with-parsed-body (docstring declares body)
    (validate-registered-fns)
    (register-fn-name name)
    `(progn
       (defun ,(unchecked-name name) ,params
         ,@declares
         (macrolet ((plet (bindings &body body)
                      `(plet/defpar ,bindings ,@body))
                    (plet-if (predicate bindings &body body)
                        `(plet-if/defpar ,predicate ,bindings ,@body))
                    ,@(registered-macrolets))
           ,@body))
       (defun ,name (&rest params)
         ,@(unsplice docstring)
         (declare (dynamic-extent params))
         (check-kernel)
         (apply (function ,(unchecked-name name)) params))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (register-fn ',name)))))
