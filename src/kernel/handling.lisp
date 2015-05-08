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

(in-package #:lparallel.kernel)

(defslots wrapped-error ()
  ((value :type condition :reader wrapped-error-value))
  (:documentation
   "This is a container for transferring an error that occurs inside
   `call-with-task-handler' to the calling thread."))

(defun wrap-error (condition)
  "Wrap an error. A non-error condition may also be wrapped, though it
will still be signaled with `error'."
  (make-wrapped-error-instance
   :value (ctypecase condition
            (symbol (make-condition condition))
            (condition condition))))

(defun unwrap-result (result)
  "In `receive-result', this is called on the stored task result. The
user receives the return value of this function."
  (declare #.*full-optimize*)
  (typecase result
    (wrapped-error
     ;; A `wrapped-error' signals an error upon being unwrapped.
     (error (wrapped-error-value result)))
    (otherwise
     ;; Most objects unwrap to themselves.
     result)))

(defmacro task-handler-bind (clauses &body body)
  "Like `handler-bind' but handles conditions signaled inside tasks
that were created in `body'."
  (let ((forms (loop for clause in clauses
                     for (name fn . more) = clause
                     do (unless (and name (symbolp name) fn (not more))
                          (error "Ill-formed binding in `task-handler-bind': ~a"
                                 clause))
                     collect `(cons ',name ,fn))))
    `(let ((*client-handlers* (list* ,@forms *client-handlers*)))
       ,@body)))

(defun invoke-transfer-error (error)
  "Equivalent to (invoke-restart 'transfer-error error).

This is a convenience function for use in `task-handler-bind'."
  (invoke-restart 'transfer-error error))

(defun condition-handler (condition)
  "Mimic the CL handling mechanism, calling handlers until one assumes
control (or not)."
  (loop for ((condition-type . handler) . rest) on *client-handlers*
        do (when (typep condition condition-type)
             (let ((*client-handlers* rest))
               (handler-bind ((condition #'condition-handler))
                 (funcall handler condition)))))
  (when (and (typep condition 'error)
             (not *debug-tasks-p*))
    (invoke-transfer-error condition)))

(defun call-with-tracked-error (condition body-fn)
  (unwind-protect/ext
   :prepare (when *worker*
              (with-lock-held (*erroring-workers-lock*)
                (push *worker* *erroring-workers*)))
   :main    (let ((*debugger-error* condition))
              (funcall body-fn))
   :cleanup (when *worker*
              (with-lock-held (*erroring-workers-lock*)
                (setf *erroring-workers*
                      (delete *worker* *erroring-workers*))))))

(defmacro with-tracked-error (condition &body body)
  `(call-with-tracked-error ,condition (lambda () ,@body)))

(defun make-debugger-hook ()
  "Record `*debugger-error*' for the `transfer-error' restart."
  (if *debugger-hook*
      (let ((previous-hook *debugger-hook*))
        (lambda (condition self)
          (with-tracked-error condition
            (funcall previous-hook condition self))))
      (lambda (condition self)
        (declare (ignore self))
        (with-tracked-error condition
          (invoke-debugger condition)))))

(defun transfer-error-report (stream)
  (format stream "Transfer this error to a dependent thread, if one exists."))

(defconstant +current-task+ 'current-task)

(defun transfer-error-restart (&optional (err *debugger-error*))
  (when err
    (throw +current-task+ (wrap-error err))))

#-lparallel.without-task-handling
(progn
  (defmacro with-task-context (&body body)
    `(catch +current-task+
       ,@body))

  (defun %call-with-task-handler (fn)
    (declare #.*full-optimize*)
    (declare (type function fn))
    (let ((*handler-active-p* t)
          (*debugger-hook* (make-debugger-hook)))
      (handler-bind ((condition #'condition-handler))
        (restart-bind ((transfer-error #'transfer-error-restart
                         :report-function #'transfer-error-report))
          (funcall fn)))))

  (defun call-with-task-handler (fn)
    (declare #.*full-optimize*)
    (declare (type function fn))
    (with-task-context
      (if *handler-active-p*
          (funcall fn)
          (%call-with-task-handler fn)))))

#+lparallel.without-task-handling
(progn
  (defmacro with-task-context (&body body) `(progn ,@body))
  (alias-function %call-with-task-handler funcall)
  (alias-function call-with-task-handler  funcall))

(define-condition task-killed-error (error) ()
  (:report
   "The task was killed.")
  (:documentation
   "Error signaled when attempting to obtain the result of a killed task."))

(define-condition no-kernel-error (error) ()
  (:report
"Welcome to lparallel. To get started, you need to create some worker
threads. Choose the MAKE-KERNEL restart to create them now.

Worker threads are asleep when not in use. They are typically created
once per Lisp session.

Adding the following line to your startup code will prevent this
message from appearing in the future (N is the number of workers):

  (setf lparallel:*kernel* (lparallel:make-kernel N))
")
  (:documentation
   "Error signaled when `*kernel*' is nil."))

(define-condition kernel-creation-error (error) ()
  (:report
   "Failed to create a kernel.")
  (:documentation
   "Error signaled when `make-kernel' fails."))
