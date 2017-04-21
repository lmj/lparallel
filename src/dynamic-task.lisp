;;; Copyright (c) 2017, James M. Lawrence. All rights reserved.
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

(defpackage #:lparallel.dynamic-task
  (:documentation
   "(private) Dynamic tasks.")
  (:use #:cl
        #:lparallel.util)
  (:import-from #:lparallel.kernel
                #:task-handler-bind)
  (:export #:define-dynamic-task
           #:dynamic-task
           #:with-dynamic-tasks
           #:*dynamic-task-vars*))

(in-package #:lparallel.dynamic-task)

(defvar *dynamic-task-vars* nil
  "The names of dynamic variables to be bound inside dynamic tasks.")

(define-condition var-info ()
  ((names :accessor var-info-names)
   (vals :accessor var-info-vals))
  (:documentation
   "A dynamic task signals this condition to obtain the values of the
variables in `*dynamic-task-vars*' in the parent thread at the time
the task was submitted."))

(defun var-info-handler ()
  "Record the values of the variables in `*dynamic-task-vars*' and
return a handler that puts these values in a `var-info' condition."
  (let* ((names (copy-list *dynamic-task-vars*))
         (vals (mapcar #'symbol-value names)))
    (lambda (condition)
      (setf (var-info-names condition) names
            (var-info-vals condition) vals))))

(defun call-with-dynamic-tasks (new-vars fn)
  (if (or *dynamic-task-vars* new-vars)
      (let ((*dynamic-task-vars* (union *dynamic-task-vars* new-vars)))
        (task-handler-bind ((var-info (var-info-handler)))
          (funcall fn)))
      (funcall fn)))

(defmacro with-dynamic-tasks (bindings &body body)
  "Tasks created within `body' will execute with the current bindings
of the variables in `*dynamic-task-vars*' in the current (parent)
thread.

Optionally add bindings with `bindings', which is a list of (variable
value) pairs as in `let'. Any variables in `bindings' that are not
included in `*dynamic-task-vars*' will be included for the scope of
`with-dynamic-tasks'."
  (let ((new-vars (mapcar #'first bindings)))
    (with-parsed-body (body declares)
      `(let ,bindings
         ,@declares
         (call-with-dynamic-tasks ',new-vars (lambda () ,@body))))))

(defun call-dynamic-task (fn)
  (let ((condition (make-condition 'var-info)))
    (signal condition)
    (progv (var-info-names condition) (var-info-vals condition)
      (funcall fn))))

(defmacro define-dynamic-task (name lambda-list &body body)
  "Define a function which, if submitted as a task within
`with-dynamic-tasks', executes with the bindings of the variables in
`*dynamic-task-vars*' from the thread in which the task was created.
The semantics are the same as `defun'."
  (with-parsed-body (body declares docstring)
    `(defun ,name ,lambda-list
       ,@(unsplice docstring)
       ,@declares
       (call-dynamic-task (lambda () ,@body)))))

(defmacro dynamic-task (lambda-list &body body)
  "Same as `define-dynamic-task' but for an unnamed lambda. The
semantics are the same as `lambda'."
  (with-parsed-body (body declares docstring)
    `(lambda ,lambda-list
      ,@(unsplice docstring)
      ,@declares
      (call-dynamic-task (lambda () ,@body)))))
