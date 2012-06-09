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

#.(import '(bordeaux-threads:destroy-thread
            bordeaux-threads:current-thread))

(defun/type exec-task/worker (task worker) (task worker) t
  ;; already inside call-with-task-handler
  (declare #.*normal-optimize*)
  (with-worker-slots (running-category) worker
    (let1 prev-category running-category
      (unwind-protect/ext
       :prepare (setf running-category (task-category task))
       :main    (funcall (task-fn task))
       :cleanup (setf running-category prev-category)))))

(defun/type exec-task/non-worker (task) (task) t
  ;; not inside call-with-task-handler
  (declare #.*normal-optimize*)
  (call-with-task-handler (task-fn task)))

(defun/type steal-work () () boolean
  (declare #.*normal-optimize*)
  (when-let (task (steal-task (scheduler *kernel*)))
    (let1 worker *worker*
      (if worker
          (exec-task/worker task worker)
          (exec-task/non-worker task)))
    t))

(defun/type handshake/to-worker (worker) (worker) t
  (with-worker-slots (handshake/from-worker handshake/to-worker) worker
    (push-queue 'proceed handshake/to-worker)
    (assert (eq 'ok (pop-queue handshake/from-worker)))))

(defun/type handshake/from-worker (worker) (worker) t
  (with-worker-slots (handshake/from-worker handshake/to-worker) worker
    (assert (eq 'proceed (pop-queue handshake/to-worker)))
    (push-queue 'ok handshake/from-worker)))

(defun/type notify-exit (worker) (worker) t
  (with-worker-slots (exit-notification) worker
    (push-queue 'exit exit-notification)))

(defun/type wait-for-worker (worker) (worker) t
  (with-worker-slots (exit-notification) worker
    (assert (eq 'exit (pop-queue exit-notification)))))

(defun/type replace-worker (kernel worker) (kernel worker) t
  (with-kernel-slots (workers workers-lock) kernel
    (with-lock-held (workers-lock)
      (let1 index (position worker workers :test #'eq)
        (assert index)
        (assert (eql index (with-worker-slots (index) worker
                             index)))
        (unwind-protect/ext
           :prepare (warn "lparallel: Replacing lost or dead worker.")
           :main    (let1 new-worker (make-worker kernel index (tasks worker))
                      (setf (svref workers index) new-worker)
                      (handshake/to-worker new-worker))
           :abort   (warn "lparallel: Worker replacement failed! ~
                           Kernel is defunct -- call `end-kernel'."))))))

(defun/type worker-loop (kernel worker) (kernel worker) t
  ;; All implementations tested so far execute unwind-protect clauses
  ;; when the ABORT restart is invoked (TERMINATE-THREAD in SBCL),
  ;; including ABCL.
  ;; 
  ;; All but ABCL execute unwind-protect for bordeaux-threads:destroy-thread.
  ;; 
  ;; This function is inside `call-with-task-handler' (or
  ;; equivalent). Jumping out means a thread abort.
  (declare #.*normal-optimize*)
  (let1 scheduler (scheduler kernel)
    (unwind-protect/ext
       :main  (loop (exec-task/worker (or (next-task scheduler worker) (return))
                                      worker))
       :abort (unless *lisp-exiting-p*
                (replace-worker kernel worker)))))

(defun/type call-with-worker-context (fn worker-context kernel)
    (function function kernel) t
  (funcall worker-context
           (lambda ()
             (let1 *worker* (find (current-thread) (workers kernel)
                                  :key #'thread)
               (assert *worker*)
               (%call-with-task-handler fn)))))

(defun/type enter-worker-loop (kernel worker) (kernel worker) t
  (with-kernel-slots (worker-info) kernel
    (with-worker-info-slots (context) worker-info
      (call-with-worker-context
       (lambda () (worker-loop kernel worker))
       context
       kernel))))

(defun make-all-bindings (kernel bindings)
  (append bindings
          (list (cons '*kernel* kernel))
          *kernel-thread-locals*))

#+lparallel.with-stealing-scheduler
(defun %make-worker (index tasks)
  (make-worker-instance :thread nil :index index :tasks tasks))

#-lparallel.with-stealing-scheduler
(defun %make-worker (index tasks)
  (declare (ignore tasks))
  (make-worker-instance :thread nil :index index))

(defun make-worker (kernel index tasks)
  (with-kernel-slots (worker-info) kernel
    (with-worker-info-slots (bindings name) worker-info
      (let* ((worker (%make-worker index tasks))
             (all-bindings (make-all-bindings kernel bindings))
             (worker-thread (with-thread (:bindings all-bindings :name name)
                              (unwind-protect/ext
                               :prepare (handshake/from-worker worker)
                               :main    (enter-worker-loop kernel worker)
                               :cleanup (notify-exit worker)))))
        (with-worker-slots (thread) worker
          (setf thread worker-thread))
        worker))))

(defgeneric make-optimizer-data (specializer)
  (:method ((specializer (eql nil)))
    (declare (ignore specializer))))

(defun make-kernel (worker-count
                    &key
                    (name "lparallel")
                    (bindings `((*standard-output* . ,*standard-output*)
                                (*error-output*    . ,*error-output*)))
                    (context #'funcall)
                    (spin-count *kernel-spin-count*))
  "Create a kernel with `worker-count' number of worker threads.

`name' is a string identifier for this kernel which is reported by
`print-object'. Worker threads will also be given this name, shown in
`bordeaux-threads:all-threads'.

`bindings' is an alist for establishing thread-local variables inside
worker threads (see bordeaux-threads for more information). By default
workers will have *standard-output* and *error-output* bindings.

Dynamic context for each worker may be established with the function
`context'. The argument passed to `context' is a function which must
be funcalled. It begins the worker loop and will not return until the
worker exits. Default value of `context' is #'funcall. The special
variables in `bindings' have been defined when `context' is called.

When a worker discovers that no tasks are available, `spin-count' is
the number of task-searching iterations done by the worker before
sleeping."
  (check-type worker-count (integer 1 #.most-positive-fixnum))
  (let* ((workers (make-array worker-count))
         (kernel  (make-kernel-instance
                   :scheduler (make-scheduler workers spin-count)
                   :workers workers
                   :workers-lock (make-lock)
                   :worker-info (make-worker-info-instance
                                 :bindings bindings
                                 :context context
                                 :name name)
                   :optimizer-data (make-optimizer-data *optimizer*))))
    (dotimes (index worker-count)
      (setf (aref workers index) (make-worker kernel index (make-spin-queue))))
    (dosequence (worker workers)
      (handshake/to-worker worker))
    kernel))

(defun check-kernel ()
  "Ensures that *kernel* is non-nil; provides the MAKE-KERNEL restart."
  (unless *kernel*
    (restart-case (error 'no-kernel-error)
      (make-kernel (worker-count)
        :report "Make a kernel now (prompt for number of workers)."
        :interactive (lambda () (interact "Enter number of workers: "))
        (setf *kernel* (make-kernel worker-count)))
      (store-value (value)
        :report "Assign a value to lparallel:*kernel*."
        :interactive (lambda () (interact "Value for lparallel:*kernel*: "))
        (setf *kernel* value))))
  nil)

(defun kernel-bindings ()
  "Return the bindings passed to `make-kernel'."
  (check-kernel)
  (with-kernel-slots (worker-info) *kernel*
    (with-worker-info-slots (bindings) worker-info
      (copy-alist bindings))))

(defun/type/inline %kernel-worker-count (kernel) (kernel) fixnum
  (length (workers kernel)))

(defun kernel-worker-count ()
  "Return the number of workers in the current kernel."
  (check-kernel)
  (%kernel-worker-count *kernel*))

(defun make-channel (&optional initial-capacity)
  "Create a channel for submitting and receiving tasks. The current
value of `*kernel*' is stored for use in `submit-task'.

As an optimization, an internal size may be given with
`initial-capacity'. This does not limit the channel size."
  (check-kernel)
  (make-channel-instance
   :kernel *kernel*
   :queue (make-queue (or initial-capacity (%kernel-worker-count *kernel*)))))

(defmacro make-task-fn (&body body)
  (with-gensyms (client-handlers body-fn)
    `(flet ((,body-fn () ,@body))
       (declare (dynamic-extent (function ,body-fn)))
       (if *client-handlers*
           (let1 ,client-handlers *client-handlers*
             (lambda ()
               (let1 *client-handlers* ,client-handlers
                 (,body-fn))))
           (lambda () (,body-fn))))))

(defun/type/inline make-task (fn) (function) task
  (make-task-instance :category *task-category* :fn fn))

(defun/type make-channeled-task (channel fn args) (channel function list) t
  (let1 queue (channel-queue channel)
    (make-task
      (make-task-fn
        (unwind-protect/ext
         ;; task handler already established inside worker threads
         :main  (push-queue (with-task-context (apply fn args)) queue)
         ;; the task handler handles everything; unwind means thread kill
         :abort (push-queue (wrap-error 'task-killed-error) queue))))))

(defun/type submit-raw-task (task kernel) (task kernel) t
  (schedule-task (scheduler kernel) task *task-priority*))

(defun/type submit-task (channel function &rest args)
    (channel (or symbol function) &rest t) t
  "Submit a task through `channel' to the kernel stored in `channel'."
  (submit-raw-task (make-channeled-task channel
                                        (ensure-function function)
                                        args)
                   (channel-kernel channel)))

(defun/type receive-result (channel) (channel) t
  "Remove a result from `channel'. If nothing is available the call
will block until a result is received."
  (unwrap-result (pop-queue (channel-queue channel))))

(defun/type try-receive-result (channel) (channel) (values t boolean)
  "Non-blocking version of `receive-result'.

If a result is available then it is returned as the primary value
in (values result t). Otherwise (values nil nil) is returned."
  (multiple-value-bind (result presentp) (try-pop-queue (channel-queue channel))
    (if presentp
        (values (unwrap-result result) t)
        (values nil nil))))

(defmacro/once do-fast-receives ((result &once channel &once count) &body body)
  "Receive `count' number of results, where `body' is cheap.

`body' will execute with each result while the channel lock is held.
`body' should be a trivial operation such as an `aref' call."
  (with-gensyms (queue)
    `(let1 ,queue (channel-queue ,channel)
       (with-locked-queue ,queue
         (repeat ,count
           (let1 ,result (unwrap-result (pop-queue/no-lock ,queue))
             ,@body))))))

#-abcl
(defun kill-tasks (task-category &key dry-run)
  "This is an expensive function which should only be used in
exceptional circumstances.

A task category is any object suitable for `eq' comparison. When a
task is submitted, it is assigned the category of
`*task-category*' (which has a default value of `:default').

`kill-tasks' rudely interrupts running tasks whose category is `eq' to
`task-category'. The corresponding worker threads are killed and
replaced.

Pending tasks are not affected.

If you don't know what to pass for `task-category' then you should
probably pass `:default', though this may kill more tasks than you
wish. Binding `*task-category*' around `submit-task' enables targeted
task killing.

If `dry-run' is nil, the function returns the number of tasks killed.

If `dry-run' is non-nil then no tasks are killed. In this case the
return value is the number of tasks that would have been killed if
`dry-run' were nil.

`kill-tasks' is not available in ABCL."
  (when *kernel*
    (when (null task-category)
      (error "task category cannot be NIL in KILL-TASKS"))
    (with-kernel-slots (workers workers-lock) *kernel*
      (with-lock-held (workers-lock)
        (let1 victims (map 'vector 
                           #'thread 
                           (remove-if-not (lambda (worker)
                                            (eq (running-category worker)
                                                task-category))
                                          workers))
          (unless dry-run
            (map nil #'destroy-thread victims))
          (length victims))))))

(defun shutdown (channel kernel)
  (let1 *task-priority* :low
    (submit-task channel (lambda ())))
  (receive-result channel)
  (with-kernel-slots (scheduler workers) kernel
    (repeat (length workers)
      (schedule-task scheduler nil :low))
    (dosequence (worker workers)
      (wait-for-worker worker))))

(defun end-kernel (&key wait)
  "Sets `*kernel*' to nil and ends all workers gracefully.

`end-kernel' should not be used as a substitute for properly waiting
on tasks with `receive-result' or otherwise.

If `wait' is nil (the default) then `end-kernel' returns immediately.
Workers are waited upon by a separate shutdown manager thread.

If `wait' is non-nil then `end-kernel' blocks until all workers are
finished. No shutdown manager thread is created.

A list of the implementation-defined worker thread objects is
returned. If `wait' is nil then the shutdown manager thread is also
returned as the first element in the list.

Note that creating and destroying kernels is relatively expensive. A
kernel typically exists for lifetime of the Lisp process. Having more
than one kernel is fine -- simply use `let' to bind a kernel instance
to `*kernel*' when you need it. Use `kill-tasks' to terminate
deadlocked or infinite looping tasks."
  (when *kernel*
    (let ((kernel *kernel*)
          (channel (make-channel)))
      (setf *kernel* nil)
      (with-kernel-slots (workers) kernel
        (let1 threads (map 'list #'thread workers)
          (cond (wait
                 (shutdown channel kernel)
                 threads)
                (t
                 (cons (with-thread (:name "lparallel kernel shutdown manager")
                         (shutdown channel kernel))
                       threads))))))))

(defun task-categories-running ()
  "Return a vector containing the task category currently running for
each worker."
  (if *kernel*
      (map 'vector #'running-category (workers *kernel*))
      #()))

(defun kernel-info (kernel)
  (with-kernel-slots (worker-info) kernel
    (with-worker-info-slots (name) worker-info
      (nconc (list :name name
                   :worker-count (%kernel-worker-count kernel))
             #+lparallel.with-stealing-scheduler
             (with-scheduler-slots (spin-count) (scheduler kernel)
               (list :spin-count spin-count))))))

(defmethod print-object ((kernel kernel) stream)
  (print-unreadable-object (kernel stream :type t :identity t)
    (format stream "~{~s~^ ~}" (kernel-info kernel))))

(alias-function emergency-kill-tasks kill-tasks :deprecate t)
(alias-function kernel-special-bindings kernel-bindings :deprecate t)
