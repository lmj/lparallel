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

(defconstant +worker-suicide-tag+ 'worker-suicide-tag)

(defun kill (kernel category)
  (assert kernel)
  (let ((kill-count 0))
    (with-kernel-slots (workers-lock workers) kernel
      (with-lock-held (workers-lock)
        (dosequence (worker workers)
          (when (and (not (eq (thread worker) (current-thread)))
                     (eql category (running-category worker)))
            (destroy-thread (thread worker))
            (incf kill-count)))
        (when *worker*
          (assert (eq (thread *worker*) (current-thread)))
          (when (eql category (running-category *worker*))
            (throw +worker-suicide-tag+ nil)))))
    kill-count))

(defun kill-errors ()
  (let ((suicide nil))
    (with-lock-held (*erroring-workers-lock*)
      (dolist (worker *erroring-workers*)
        (if (and *worker* (eq worker *worker*))
            (setf suicide t)
            ;; user could possibly (though unlikely) destroy the
            ;; thread simultaneously, so ignore double-destroy error
            (ignore-errors (destroy-thread (thread worker)))))
      (when suicide
        (assert (eq (thread *worker*) (current-thread)))
        (throw +worker-suicide-tag+ nil)))))

(defun kill-errors-report (stream)
  (format stream "Kill errors in workers (remove debugger instances)."))

(defmacro with-worker-restarts (&body body)
  `(catch +worker-suicide-tag+
     (restart-bind ((kill-errors #'kill-errors
                      :report-function #'kill-errors-report))
       ,@body)))

(defun kill-tasks (task-category &key dry-run)
  "This is an expensive function which should only be used in
exceptional circumstances.

Every task has an associated task category. When a task is submitted,
it is assigned the category of `*task-category*' which has a default
value of `:default'.

`kill-tasks' interrupts running tasks whose category is `eql' to
 `task-category'. The corresponding worker threads are killed and
 replaced. Pending tasks are not affected.

If you don't know what to pass for `task-category' then you should
probably pass `:default', though this may kill more tasks than you
wish. Binding `*task-category*' around `submit-task' enables targeted
task killing.

If `dry-run' is nil, the function returns the number of tasks killed.

If `dry-run' is non-nil then no tasks are killed. In this case the
return value is the number of tasks that would have been killed if
`dry-run' were nil.

`kill-tasks' is not available in ABCL."
  (let ((kernel *kernel*))
    (when kernel
      (unless task-category
        (error "Task category cannot be nil in `kill-tasks'."))
      (if dry-run
          (count task-category (workers kernel) :key #'running-category)
          (kill kernel task-category)))))
