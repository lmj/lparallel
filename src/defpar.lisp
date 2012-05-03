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
;;; defpar
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *optimizer* 'defpar)

(defslots task-counter ()
  ((task-count              :initform 0 :type fixnum)
   (lock       :reader lock :initform (make-lock))))

(defmethod make-optimizer-data ((specializer (eql 'defpar)))
  (make-task-counter-instance))

(defun/type update-task-count/no-lock (kernel delta) (kernel fixnum) t
  (declare #.*normal-optimize*)
  (with-optimizer-slots (optimizer-data optimizer-flag) kernel
    (with-task-counter-slots (task-count) optimizer-data
      (incf task-count delta)
      ;; optimizer-flag == accept-task-p
      ;; convert to strict boolean -- '<' is generalized boolean
      (setf optimizer-flag (not (>= task-count
                                    (1+ (%kernel-worker-count kernel))))))))

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
      (declare (optimize (speed 3) (safety 0) (debug 1)))
      (if (< n 2)
          n
          (plet ((a (fib (- n 1)))
                 (b (fib (- n 2))))
            (+ a b))))

WARNING: Unlike the rest of the lparallel API, `defpar' does not call
`check-kernel' for you. You are responsible for calling `check-kernel'
or otherwise ensuring that `*kernel*' is bound to a kernel.

NOTE: `defpar' may require a high optimization level in order to
produce significant speedup."
  (with-parsed-body (preamble body :docstring t)
    `(defun ,name ,params
       ,@preamble
       (macrolet ((plet (bindings &body body)
                    `(plet/defpar ,bindings ,@body))
                  (plet-if (predicate bindings &body body)
                    `(plet-if/defpar ,predicate ,bindings ,@body)))
         ,@body))))
