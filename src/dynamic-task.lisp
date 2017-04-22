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
  "A list of dynamic variables to be bound inside dynamic tasks.")

;;; A dynamic task signals this condition to obtain the values of the
;;; variables in `*dynamic-task-vars*' in the parent thread at the
;;; time `with-dynamic-tasks' was called.
(define-condition get-vars () ())

;;; Record the values of the variables in `*dynamic-task-vars*' and
;;; return a handler that provides these values to dynamic tasks.
(defun make-get-vars-handler ()
  (let* ((vars (copy-list *dynamic-task-vars*))
         (vals (mapcar #'symbol-value vars)))
    (lambda (condition)
      (declare (ignore condition))
      (invoke-restart 'continue vars vals))))

(defun call-with-dynamic-tasks (new-vars fn)
  (if (or *dynamic-task-vars* new-vars)
      (let ((*dynamic-task-vars* (union *dynamic-task-vars* new-vars)))
        (task-handler-bind ((get-vars (make-get-vars-handler)))
          (funcall fn)))
      (funcall fn)))

(defmacro with-dynamic-tasks (bindings &body body)
  "Tasks created within `body' execute with the current bindings of
the variables in `*dynamic-task-vars*' from the current thread.

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
  (restart-case (progn
                  (signal 'get-vars)
                  ;; not submitted inside `with-dynamic-tasks'
                  (funcall fn))
    (continue (vars vals)
      (progv vars vals
        (funcall fn)))))

(defmacro defdef (definer &optional name)
  `(with-parsed-body (body declares docstring)
    `(,',definer ,,@(unsplice name) ,lambda-list
       ,@(unsplice docstring)
       ,@declares
       (call-dynamic-task (lambda () ,@body)))))

(defmacro define-dynamic-task (name lambda-list &body body)
  "Define a function which, if submitted as a task within
`with-dynamic-tasks', executes with the bindings of the variables in
`*dynamic-task-vars*' at the point of the `with-dynamic-tasks' form in
the thread in which the task was created. The semantics are the same
as `defun'."
  (defdef defun name))

(defmacro dynamic-task (lambda-list &body body)
  "Like `define-dynamic-task' but for an unnamed lambda. The semantics
are the same as `lambda'."
  (defdef lambda))
