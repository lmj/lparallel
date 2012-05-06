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
  ((object :type condition))
  (:documentation
   "This is a container for transferring an error that occurs inside
   `call-with-task-handler' to the calling thread."))

(defun wrap-error (err)
  (make-wrapped-error-instance
   :object (etypecase err
             (symbol (make-condition err))
             (error  err))))

(defgeneric unwrap-result (result)
  (:documentation
   "In `receive-result', this is called on the stored task result.
   The user receives the return value of this function."))

(defmethod unwrap-result (result)
  "Most objects unwrap to themselves."
  result)

(defmethod unwrap-result ((result wrapped-error))
  "A `wrapped-error' signals an error upon being unwrapped."
  (with-wrapped-error-slots (object) result
    (error object)))

(defmacro task-handler-bind (clauses &body body)
  "Like `handler-bind' but reaches into kernel worker threads."
  (let1 forms (loop
                 :for clause :in clauses
                 :for (name fn more) := clause
                 :do (unless (and (symbolp name) (not more))
                       (error "Wrong format in TASK-HANDLER-BIND clause: ~a"
                              clause))
                 :collect `(cons ',name ,fn))
    `(let1 *client-handlers* (nconc (list ,@forms) *client-handlers*)
       ,@body)))

(defun condition-handler (con)
  "Mimic the CL handling mechanism, calling handlers until one assumes
control (or not)."
  (loop
     :for (name . fn)  :in *client-handlers*
     :for (nil . tail) :on *client-handlers*
     :do (when (subtypep (type-of con) name)
           (let1 *client-handlers* tail
             (handler-bind ((condition #'condition-handler))
               (funcall fn con))))))

(defun make-debugger-hook ()
  "Record `*debugger-error*' for the `transfer-error' restart."
  (if *debugger-hook*
      (let1 previous-hook *debugger-hook*
        (lambda (condition self)
          (let1 *debugger-error* condition
            (funcall previous-hook condition self))))
      (lambda (condition self)
        (declare (ignore self))
        (let1 *debugger-error* condition
          (invoke-debugger condition)))))

(defun transfer-error-report (stream)
  (format stream "Transfer this error to dependent threads, if any."))

(defun transfer-error-restart (&optional (err *debugger-error*))
  (throw 'current-task
    (make-wrapped-error-instance
     :object (ctypecase err
               (condition err)
               (symbol (make-condition err))))))

#-lparallel.without-task-handling
(progn
  (defmacro with-task-context (&body body)
    `(catch 'current-task
       ,@body))

  (defun %call-with-task-handler (fn)
    (let ((*handler-active-p* t)
          (*debugger-hook* (make-debugger-hook)))
      (handler-bind ((condition #'condition-handler))
        (restart-bind ((transfer-error #'transfer-error-restart
                         :report-function #'transfer-error-report))
          (funcall fn)))))

  (defun/type call-with-task-handler (fn) (function) (values &rest t)
    (declare #.*normal-optimize*)
    (with-task-context
      (if *handler-active-p*
          (funcall fn)
          (%call-with-task-handler fn)))))

#+lparallel.without-task-handling
(progn
  (defmacro with-task-context (&body body) `(progn ,@body))
  (alias-function %call-with-task-handler funcall)
  (alias-function call-with-task-handler  funcall))

(define-condition task-killed-error (error) ())

(define-condition no-kernel-error (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
"Welcome to lparallel. To get started, you need to create some worker
threads. Choose the MAKE-KERNEL restart to create them now.

Worker threads are asleep when not in use. They are typically created
once per Lisp session.

Adding the following line to your startup code will prevent this
message from appearing in the future (N is the number of workers):

  (setf lparallel:*kernel* (lparallel:make-kernel N))
")))
  (:documentation "Error signaled when `*kernel*' is nil."))

;;; deprecated
(setf (macro-function 'kernel-handler-bind) (macro-function 'task-handler-bind))