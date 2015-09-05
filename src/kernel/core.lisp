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

#-lparallel.without-task-categories
(defun/type exec-task/worker (task worker) (task worker) t
  ;; already inside call-with-task-handler
  (declare #.*full-optimize*)
  (with-worker-slots (running-category) worker
    (let ((prev-category running-category))
      (unwind-protect/ext
       :prepare (setf running-category (task-category task))
       :main    (funcall (task-fn task))
       :cleanup (setf running-category prev-category)))))

#+lparallel.without-task-categories
(defun/type/inline exec-task/worker (task worker) (task worker) t
  (declare #.*full-optimize*)
  (declare (ignore worker))
  (funcall (task-fn task)))

(defun/type/inline exec-task/non-worker (task) (task) t
  ;; not inside call-with-task-handler
  (declare #.*full-optimize*)
  (call-with-task-handler (task-fn task)))

(defun/type steal-work (kernel worker) (kernel (or worker null)) boolean
  (declare #.*full-optimize*)
  (when-let (task (steal-task (scheduler kernel)))
    (if worker
        (exec-task/worker task worker)
        (exec-task/non-worker task))
    t))

(defun handshake/to-worker/start (worker)
  (with-worker-slots (handshake/to-worker) worker
    (push-queue 'proceed handshake/to-worker)))

(defun handshake/to-worker/finish (worker)
  (with-worker-slots (handshake/from-worker) worker
    (ecase (pop-queue handshake/from-worker)
      (ok)
      (error (error 'kernel-creation-error)))))

(defun handshake/from-worker/start (worker)
  (with-worker-slots (handshake/to-worker) worker
    (assert (eq 'proceed (pop-queue handshake/to-worker)))))

(defun handshake/from-worker/finish (worker status)
  (check-type status (member ok error))
  (with-worker-slots (handshake/from-worker) worker
    (push-queue status handshake/from-worker)))

(defun notify-exit (worker)
  (with-worker-slots (exit-notification) worker
    (push-queue 'exit exit-notification)))

(defun wait-for-worker (worker)
  (with-worker-slots (exit-notification) worker
    (assert (eq 'exit (pop-queue exit-notification)))))

(defun replace-worker (kernel worker)
  (with-kernel-slots (workers workers-lock) kernel
    (with-lock-held (workers-lock)
      (let ((index (position worker workers :test #'eq)))
        (assert index)
        (assert (eql index (worker-index worker)))
        (unwind-protect/ext
           :prepare (warn "lparallel: Replacing lost or dead worker.")
           :main    (let ((new-worker (make-worker kernel index
                                                   (tasks worker))))
                      (setf (svref workers index) new-worker)
                      (handshake/to-worker/start new-worker)
                      (handshake/to-worker/finish new-worker))
           :abort   (warn "lparallel: Worker replacement failed! ~
                           Kernel is defunct."))))))

(defun/type worker-loop (kernel worker) (kernel worker) t
  ;; All implementations tested so far execute unwind-protect clauses
  ;; when the ABORT restart is invoked (TERMINATE-THREAD in SBCL),
  ;; including ABCL.
  ;;
  ;; All but ABCL execute unwind-protect for destroy-thread.
  ;;
  ;; This function is inside `call-with-task-handler' (or
  ;; equivalent). Jumping out means a thread abort.
  (declare #.*full-optimize*)
  (let ((scheduler (scheduler kernel)))
    (unwind-protect/ext
       :main  (loop (let ((task (next-task scheduler worker)))
                      (if task
                          (exec-task/worker (the task task) worker)
                          (return))))
       :abort (unless *lisp-exiting-p*
                (replace-worker kernel worker)))))

#+lparallel.without-kill
(defmacro with-worker-restarts (&body body)
  `(progn ,@body))

(defun call-with-worker-context (fn worker-context kernel worker)
  (handshake/from-worker/start worker)
  (unwind-protect
       (funcall worker-context
                (lambda ()
                  (let ((*worker* (find (current-thread) (workers kernel)
                                        :key #'thread)))
                    (assert *worker*)
                    (handshake/from-worker/finish worker 'ok)
                    (with-worker-restarts
                      (%call-with-task-handler fn)))))
    ;; This error notification is seen when `worker-context' does not
    ;; call its worker-loop parameter, otherwise it's ignored.
    (handshake/from-worker/finish worker 'error)))

(defun enter-worker-loop (kernel worker)
  (with-kernel-slots (worker-info) kernel
    (with-worker-info-slots (context) worker-info
      (call-with-worker-context
       (lambda () (worker-loop kernel worker))
       context
       kernel
       worker))))

(defun make-all-bindings (kernel bindings)
  (append bindings (list (cons '*kernel* kernel))))

#+lparallel.with-stealing-scheduler
(defun %make-worker (index tasks)
  (make-worker-instance :thread nil :index index :tasks tasks))

#-lparallel.with-stealing-scheduler
(defun %make-worker (index tasks)
  (declare (ignore tasks))
  (make-worker-instance :thread nil :index index))

(defun make-worker-thread (kernel worker name bindings)
  (with-thread (:name name :bindings bindings)
    (unwind-protect/ext
     :main    (enter-worker-loop kernel worker)
     :cleanup (notify-exit worker))))

(defun make-worker (kernel index tasks)
  (with-kernel-slots (worker-info) kernel
    (with-worker-info-slots (bindings name) worker-info
      (let* ((worker (%make-worker index tasks))
             (bindings (make-all-bindings kernel bindings))
             (worker-thread (make-worker-thread kernel worker name bindings)))
        (with-worker-slots (thread) worker
          (setf thread worker-thread))
        worker))))

(defmacro with-fill-workers-handler (workers &body body)
  `(unwind-protect/ext
    :main  (progn ,@body)
    :abort (dosequence (worker ,workers)
             (when (typep worker 'worker)
               (ignore-errors (destroy-thread (thread worker)))))))

(defun %fill-workers (workers kernel)
  (dotimes (index (length workers))
    (setf (aref workers index)
          (make-worker kernel
                       index
                       #+lparallel.with-stealing-scheduler (make-spin-queue)
                       #-lparallel.with-stealing-scheduler nil))))

(defun fill-workers (workers kernel)
  ;; Start/finish calls are separated for parallel initialization.
  ;;
  ;; Ensure that each worker calls its worker-loop parameter,
  ;; otherwise an error is signaled, whereupon all workers are killed.
  ;;
  ;; If a `make-thread' call fails (e.g. too many threads) then all
  ;; workers are killed.
  (with-fill-workers-handler workers
    (%fill-workers workers kernel)
    (map nil #'handshake/to-worker/start workers)
    (map nil #'handshake/to-worker/finish workers)))

(defun make-kernel (worker-count
                    &key
                    (name "lparallel")
                    (bindings `((*standard-output* . ,*standard-output*)
                                (*error-output*    . ,*error-output*)))
                    (context #'funcall)
                    (spin-count *kernel-spin-count*)
                    (use-caller nil))
  "Create a kernel with `worker-count' number of worker threads.

`name' is a string identifier for this kernel which is reported by
`print-object'. Worker threads will also be given this name, shown in
`bordeaux-threads:all-threads'.

`bindings' is an alist for establishing thread-local variables inside
worker threads. By default workers will have *standard-output* and
*error-output* bindings.

Dynamic context for each worker may be established with the function
`context'. The argument passed to `context' is a function which must
be funcalled. It begins the worker loop and will not return until the
worker exits. The default value of `context' is #'funcall. The special
variables in `bindings' are available inside the `context' function.

When a worker discovers that no tasks are available, `spin-count' is
the number of task-searching iterations done by the worker before
sleeping.

If `use-caller' is true (default is false) then in certain situations
the calling thread may be enlisted to steal work from worker threads.
This is an optimization strategy that currently applies only during
the execution of functions defined by `defpun' and `defpun/type'.
Typically in this case the number of workers will be one less than the
number of cores/CPUs.

A kernel will not be garbage collected until `end-kernel' is called."
  (check-type worker-count (integer 1 #.most-positive-fixnum))
  (check-type spin-count index)
  (let* ((workers (make-array worker-count))
         (thread-count (if use-caller (1+ worker-count) worker-count))
         (kernel (apply #'make-kernel-instance
                        :scheduler (make-scheduler workers spin-count)
                        :workers workers
                        :workers-lock (make-lock)
                        :worker-info (make-worker-info-instance
                                      :bindings bindings
                                      :context (ensure-function context)
                                      :name name)
                        :use-caller-p (to-boolean use-caller)
                        :alivep t
                        (funcall *make-limiter-data* thread-count))))
    (fill-workers workers kernel)
    kernel))

(defun check-kernel ()
  "Ensures the value of `*kernel*' is a kernel instance. Provides the
MAKE-KERNEL and STORE-VALUE restarts. Returns `*kernel*'."
  (or *kernel*
      (restart-case (error 'no-kernel-error)
        (make-kernel (worker-count)
          :report "Make a kernel now (prompt for number of workers)."
          :interactive (lambda () (interact "Enter number of workers: "))
          (setf *kernel* (make-kernel worker-count)))
        (store-value (value)
          :report "Assign a value to lparallel:*kernel*."
          :interactive (lambda () (interact "Value for lparallel:*kernel*: "))
          (check-type value kernel)
          (setf *kernel* value)))))

(defmacro define-worker-info-reader (name slot &optional (result slot))
  `(defun ,name ()
     ,(format nil "Return the ~a passed to `make-kernel'."
              (string-downcase slot))
     (with-kernel-slots (worker-info) (check-kernel)
       (with-worker-info-slots (,slot) worker-info
         ,result))))

(define-worker-info-reader kernel-bindings bindings (copy-alist bindings))
(define-worker-info-reader kernel-name name)
(define-worker-info-reader kernel-context context)

(defun/type/inline %kernel-worker-count (kernel) (kernel) index
  (declare #.*full-optimize*)
  (length (workers kernel)))

(defun kernel-worker-count ()
  "Return the number of workers in the current kernel."
  (%kernel-worker-count (check-kernel)))

(defun kernel-worker-index ()
  "If called from inside a worker, return the worker's assigned index,
ranging from 0 to one less than (kernel-worker-count).

If not called from inside a worker, return nil."
  (let ((worker *worker*))
    (if worker
        (worker-index worker)
        nil)))

(defun %make-channel (&key fixed-capacity)
  (make-channel-instance
   :kernel (check-kernel)
   :queue (make-queue :fixed-capacity fixed-capacity)))

(defun make-channel (&rest args)
  "Create a channel for submitting and receiving tasks. The current
value of `*kernel*' is stored for use in `submit-task'.

By default there is no limit on the channel capacity. Passing a
`fixed-capacity' keyword argument limits the capacity to the value
passed.

Note that a fixed capacity channel may cause a deadlocked kernel if
`receive-result' is not called a sufficient number of times."
  (apply #'%make-channel (if (= 1 (length args))
                             nil
                             args)))

(define-compiler-macro make-channel (&whole whole &rest args)
  (when (= 1 (length args))
    (simple-style-warning
     "Calling `make-channel' with one argument is deprecated.~%~
      Pass no arguments instead."))
  whole)

#-lparallel.without-task-handling
(defmacro task-lambda (&body body)
  (with-gensyms (body-fn client-handlers)
    `(flet ((,body-fn () ,@body))
       (declare #.*full-optimize*)
       (let ((,client-handlers *client-handlers*))
         (if ,client-handlers
             (lambda ()
               (let ((*client-handlers* ,client-handlers))
                 (,body-fn)))
             #',body-fn)))))

#+lparallel.without-task-handling
(defmacro task-lambda (&body body)
  `(lambda () ,@body))

#-lparallel.without-task-categories
(defun/type/inline make-task (fn) (function) task
  (declare #.*full-optimize*)
  (make-task-instance fn *task-category*))

(defun/type make-channeled-task (channel fn args) (channel function list) t
  ;; avoid allocation from extent checks with safety 0 (sbcl)
  (declare #.*full-optimize*)
  (let ((queue (channel-queue channel)))
    (make-task
      (task-lambda
        (unwind-protect/ext
         ;; task handler already established inside worker threads
         :main  (push-queue (with-task-context (apply fn args)) queue)
         ;; the task handler handles everything; unwind means thread kill
         :abort (push-queue (wrap-error 'task-killed-error) queue))))))

(defun/type/inline submit-raw-task (task kernel) (task kernel) (values)
  (declare #.*normal-optimize*)
  (unless (alivep kernel)
    (error "Attempted to submit a task to an ended kernel."))
  (schedule-task (scheduler kernel) task *task-priority*)
  (values))

(defun submit-task (channel function &rest args)
  "Submit a task through `channel' to the kernel stored in `channel'."
  (declare #.*normal-optimize*)
  (check-type channel channel)
  (submit-raw-task (make-channeled-task channel
                                        (ensure-function function)
                                        args)
                   (channel-kernel channel)))

(defun receive-result (channel)
  "Remove a result from `channel'. If nothing is available the call
will block until a result is received."
  (unwrap-result (pop-queue (channel-queue channel))))

(defun try-receive-result (channel &key timeout)
  "If `channel' has a result then remove it and return (values result t).

If no result is available and `timeout' is given, then wait up to
`timeout' seconds for a result.

If the channel is empty and the timeout has expired, or if the channel
is empty and no timeout was given, return (values nil nil).

Providing a nil or non-positive value of `timeout' is equivalent to
providing no timeout."
  (multiple-value-bind (result presentp)
      (try-pop-queue (channel-queue channel) :timeout timeout)
    (if presentp
        (values (unwrap-result result) t)
        (values nil nil))))

(defmacro/once do-fast-receives ((result &once channel count) &body body)
  "Receive `count' number of results from `channel', executing `body'
each time with the result bound to `result'.

`body' should be a trivial operation such as an aref call."
  `(repeat ,count
     (let ((,result (receive-result ,channel)))
       ,@body)))

(defun shutdown (channel kernel)
  (let ((*task-priority* :low))
    (submit-task channel (lambda ())))
  (receive-result channel)
  (with-kernel-slots (scheduler workers alivep) kernel
    (repeat (length workers)
      (schedule-task scheduler nil :low))
    (map nil #'wait-for-worker workers)
    (setf alivep nil)))

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
  (let ((kernel *kernel*))
    (when kernel
      (setf *kernel* nil)
      (when (alivep kernel)
        (let ((channel (let ((*kernel* kernel)) (make-channel)))
              (threads (map 'list #'thread (workers kernel))))
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
  (let ((kernel *kernel*))
    (if kernel
        (map 'vector #'running-category (workers kernel))
        #())))

(defun kernel-info (kernel)
  (with-kernel-slots (worker-info alivep use-caller-p) kernel
    (with-worker-info-slots (name) worker-info
      (nconc (list :name name
                   :worker-count (%kernel-worker-count kernel)
                   :use-caller use-caller-p
                   :alive alivep)
             #+lparallel.with-stealing-scheduler
             (with-scheduler-slots (spin-count) (scheduler kernel)
               (list :spin-count spin-count))))))

(defmethod print-object ((kernel kernel) stream)
  (print-unreadable-object (kernel stream :type t :identity t)
    (format stream "~{~s~^ ~}" (kernel-info kernel))))

(defun broadcast-task (function &rest args)
  "Wait for current and pending tasks to complete, if any, then
simultaneously execute the given task inside each worker. Wait until
these tasks finish, then return the results in a vector.

Calling `broadcast-task' from inside a worker is an error."
  (when *worker*
    (error "Cannot call `broadcast-task' from inside a worker."))
  (let* ((function (ensure-function function))
         (*kernel* (check-kernel))
         (worker-count (kernel-worker-count))
         (channel (make-channel))
         ;; TODO: replace queues with semaphores
         (from-workers (make-queue))
         (to-workers (make-queue)))
    (repeat worker-count (submit-task channel (lambda ()
                                                (push-queue t from-workers)
                                                (pop-queue to-workers)
                                                (apply function args))))
    (repeat worker-count (pop-queue from-workers))
    (repeat worker-count (push-queue t to-workers))
    (map-into (make-array worker-count) (lambda () (receive-result channel)))))

(defun track-exit ()
  (setf *lisp-exiting-p* t))

#+sbcl    (pushnew 'track-exit sb-ext:*exit-hooks*)
#+ccl     (pushnew 'track-exit ccl:*lisp-cleanup-functions*)
#+allegro (pushnew '(track-exit) sys:*exit-cleanup-forms* :test #'equal)

;;; ccl:save-application calls ccl:*lisp-cleanup-functions* before
;;; saving. Adjust with a save hook.
#+ccl
(progn
  (defun save-hook () (setf *lisp-exiting-p* nil))
  (pushnew 'save-hook ccl:*save-exit-functions*))
