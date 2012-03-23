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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; util
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro enhanced-unwind-protect (&key prepare main cleanup abort)
  "Keywordized version of `unwind-protect' with an added `abort'
clause which is executed when the `main' clause does not finish."
  (with-gensyms (finishedp)
    `(let1 ,finishedp nil
       ,@(unsplice prepare)
       (unwind-protect (progn  ; m-v-b-prog1 in real life
                         ,main
                         (setf ,finishedp t))
         (if ,finishedp
             ,cleanup
             (unwind-protect ,abort
               ,cleanup))))))

(defmacro make-tuple (&rest forms)
  `(lambda () (values ,@forms)))

(defmacro bind-tuple (vars tuple &body body)
  `(multiple-value-bind ,vars (funcall ,tuple)
     ,@body))

(defmacro define-circular-print-object (type)
  `(defmethod print-object :around ((object ,type) stream)
     (declare (ignore object stream))
     (let1 *print-circle* t
       (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; error handling
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debugger-lock* (make-recursive-lock)
  "Global. For convenience -- allows only one debugger prompt at a
time from errors signaled inside `call-with-kernel-handler'.")

;;; This does not entail all thread locals -- only those with simple
;;; initial values.
(define-thread-locals *kernel-thread-locals*
  (*debugger-error* nil
   "Thread-local. Track the error inside the debugger for the
    `transfer-error' restart.")
  
  (*handler-active-p* nil
   "Thread-local. Non-nil when handlers have been established via
    `call-with-kernel-handler'.")

  (*client-handlers* nil
   "Thread-local. Records handlers established with
    `kernel-handler-bind' in the calling thread.")

  (*kernel-task-category* :default
   "Thread-local. See `kill-tasks'. Default value is `:default'.")

  (*kernel-task-priority* :default
   "Thread-local. When bound to `:low', the kernel schedules submitted
    tasks at low priority. Default value is `:default'."))

(defslots wrapped-error ()
  ((object :type condition))
  (:documentation
   "This is a container for transferring an error that occurs inside
   `call-with-kernel-handler' to the calling thread."))

(defun wrap-error (error-name)
  (make-wrapped-error-instance :object (make-condition error-name)))

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

(defmacro kernel-handler-bind (clauses &body body)
  "Like `handler-bind' but reaches into kernel worker threads."
  `(let ((*client-handlers*
          (nconc (list
                  ,@(loop
                       :for clause :in clauses
                       :for (name fn more) := clause
                       :do (unless (and (symbolp name) (not more))
                             (error "Wrong format in `kernel-handler-bind' ~
                                    clause: ~a"
                                    clause))
                       :collect `(cons ',name ,fn)))
                 *client-handlers*)))
     ,@body))

(defun condition-handler (con)
  "Mimic the CL handling mechanism, calling handlers until one assumes
control (or not)."
  (loop
     :for (name . fn) :in *client-handlers*
     :for pos         :on *client-handlers*
     :do (when (subtypep (type-of con) name)
           (let1 *client-handlers* (cdr pos)
             (handler-bind ((condition #'condition-handler))
               (funcall fn con))))))

(defun make-debugger-hook ()
  "Allow one debugger prompt at a time from worker threads."
  (if *debugger-hook*
      (let1 previous-hook *debugger-hook*
        (lambda (condition self)
          (let1 *debugger-error* condition
            (with-recursive-lock-held (*debugger-lock*)
              (funcall previous-hook condition self)))))
      (lambda (condition self)
        (declare (ignore self))
        (let1 *debugger-error* condition
          (with-recursive-lock-held (*debugger-lock*)
            (invoke-debugger condition))))))

(defmacro with-task-context (&body body)
  `(catch 'current-task
     ,@body))

(defun transfer-error-restart (&optional (err *debugger-error*))
  (throw 'current-task
    (make-wrapped-error-instance
     :object (ctypecase err
               (condition err)
               (symbol (make-condition err))))))

(defun transfer-error-report (stream)
  (format stream "Transfer this error to dependent threads, if any."))

(defun %call-with-kernel-handler (fn)
  (let ((*handler-active-p* t)
        (*debugger-hook* (make-debugger-hook)))
    (handler-bind ((condition #'condition-handler))
      (restart-bind ((transfer-error #'transfer-error-restart
                       :report-function #'transfer-error-report))
        (funcall fn)))))

(defun call-with-kernel-handler (fn)
  (with-task-context
    (if *handler-active-p*
        (funcall fn)
        (%call-with-kernel-handler fn))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; kernel
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots kernel ()
  ((tasks           :reader tasks   :type biased-queue)
   (workers         :reader workers :type vector)
   (workers-lock)
   (worker-bindings                 :type list)
   (worker-context                  :type function)
   (worker-name                     :type string)))

(defslots worker ()
  ((thread           :reader thread)
   (running-category :reader running-category :initform nil)))

(defslots channel ()
  ((queue  :reader channel-queue  :type queue)
   (kernel :reader channel-kernel :type kernel)))

(defvar *kernel* nil "Thread-local. The current kernel, or nil.")

(define-circular-print-object kernel)

(defun replace-worker (kernel worker)
  (with-kernel-slots (workers workers-lock) kernel
    (with-lock-held (workers-lock)
      (let1 index (position (thread worker) workers :key #'thread :test #'eq)
        (assert index)
        (enhanced-unwind-protect
           :prepare (warn "lparallel: Replacing lost or dead worker.")
           :main    (setf (aref workers index) (make-worker kernel))
           :abort   (warn "lparallel: Worker replacement failed! ~
                           Kernel is defunct -- call `end-kernel'."))))))

(defun worker-loop (kernel worker)
  ;; All implementations tested so far execute unwind-protect clauses
  ;; when the ABORT restart is invoked (TERMINATE-THREAD in SBCL),
  ;; including ABCL.
  ;; 
  ;; All but ABCL execute unwind-protect for bordeaux-threads:destroy-thread.
  ;; 
  ;; This function is inside `call-with-kernel-handler' (or
  ;; equivalent). Jumping out means a thread abort.
  (let1 tasks (tasks kernel)
    (enhanced-unwind-protect
       :main  (loop (let1 task (or (pop-biased-queue tasks) (return))
                      (with-worker-slots (running-category) worker
                        (bind-tuple (client-fn kill-notify-fn category) task
                          (enhanced-unwind-protect
                             :prepare (setf running-category category)
                             :main    (funcall client-fn)
                             :cleanup (setf running-category nil)
                             :abort   (funcall kill-notify-fn))))))
       :abort (replace-worker kernel worker))))

(defun call-with-worker-context (worker-context fn)
  (funcall worker-context (lambda () (%call-with-kernel-handler fn))))

(defun enter-worker-loop (kernel worker)
  (with-kernel-slots (worker-context) kernel
    (call-with-worker-context
     worker-context
     (lambda ()
       (worker-loop kernel worker)))))

(defun make-worker (kernel)
  (with-kernel-slots (worker-bindings worker-name) kernel
    (let* ((worker (make-worker-instance :thread nil))
           (blocker (make-queue))
           (worker-thread (with-thread (:bindings worker-bindings
                                        :name worker-name)
                            (pop-queue blocker)
                            (enter-worker-loop kernel worker))))
      (with-worker-slots (thread) worker
        (setf thread worker-thread))
      (push-queue 'proceed blocker)
      worker)))

(defun make-kernel (worker-count
                    &key
                    (bindings (list (cons '*standard-output* *standard-output*)
                                    (cons '*error-output* *error-output*)))
                    (worker-context #'funcall)
                    (name "lparallel-worker"))
  "Create a kernel with `worker-count' number of worker threads.

`bindings' is an alist for establishing thread-local variables inside
worker threads (see bordeaux-threads for more information). By default
workers will have *standard-output* and *error-output* bindings.

Dynamic context for each worker may be established with the function
`worker-context'. The argument passed to `worker-context' is a
function which must be funcalled. It begins the worker loop and will
not return until the worker exits. Default value of `worker-context'
is #'funcall.

`name' is a string identifier for worker threads. It corresponds to
the string returned by `bordeaux-threads:thread-name'."
  (check-type worker-count (integer 1))
  (let1 kernel (make-kernel-instance
                :tasks (make-biased-queue 64)
                :workers (make-array worker-count)
                :workers-lock (make-lock)
                :worker-bindings (nconc (copy-alist bindings)
                                        (acons '*debugger-hook* *debugger-hook*
                                               nil)
                                        (copy-alist *kernel-thread-locals*))
                :worker-context worker-context
                :worker-name name)
    (with-kernel-slots (workers worker-bindings) kernel
      (setf worker-bindings (acons '*kernel* kernel worker-bindings))
      (map-into workers (lambda () (make-worker kernel))))
    kernel))

(defun check-kernel ()
  "Ensure that *kernel* is non-nil."
  (unless *kernel*
    (restart-case (error 'no-kernel-error)
      (make-kernel (arg)
        :report "Make a kernel now (prompt for number of workers)."
        :interactive (lambda () (interact "Enter number of workers: "))
        (setf *kernel* (apply #'make-kernel (mklist arg))))
      (store-value (value)
        :report "Assign a value to lparallel:*kernel*."
        :interactive (lambda () (interact "Value for lparallel:*kernel: "))
        (setf *kernel* value)))))

(defun kernel-special-bindings ()
  "Return an alist of thread-local special variable bindings.
A new thread which uses the current kernel should be given these
bindings (see bordeaux-threads:*default-special-bindings*)."
  (check-kernel)
  (with-kernel-slots (worker-bindings) *kernel*
    (copy-alist worker-bindings)))

(defun kernel-worker-count ()
  "Return the number of workers in the current kernel."
  (check-kernel)
  (length (workers *kernel*)))

(defun make-channel (&optional initial-capacity)
  "Create a channel for submitting and receiving tasks. The current
value of `*kernel*' is stored for use in `submit-task'.

As an optimization, an internal size may be given with
`initial-capacity'. This does not limit the channel size."
  (check-kernel)
  (make-channel-instance
   :kernel *kernel*
   :queue (make-queue (or initial-capacity
                          (length (workers *kernel*))))))

(defmacro make-client-fn (&body body)
  (with-gensyms (client-handlers)
    `(if *client-handlers*
         (let1 ,client-handlers *client-handlers*
           (lambda ()
             (let1 *client-handlers* ,client-handlers
               ,@body)))
         (lambda ()
           ,@body))))

(defmacro make-task (&key client-fn store-error)
  (with-gensyms (task-category)
    `(let ((,task-category *kernel-task-category*))
       (make-tuple ,client-fn
                   (lambda () (,store-error (wrap-error 'task-killed-error)))
                   ,task-category))))

(defun make-channeled-task (channel fn args)
  (let1 queue (channel-queue channel)
    (macrolet ((store (code) `(push-queue ,code queue)))
      (let1 client-fn (make-client-fn
                        ;; handler already established inside
                        ;; worker threads
                        (store (with-task-context (apply fn args))))
        (make-task :client-fn client-fn :store-error store)))))

(defun submit-raw-task (task kernel)
  (ccase *kernel-task-priority*
    (:default (push-biased-queue     task (tasks kernel)))
    (:low     (push-biased-queue/low task (tasks kernel)))))

(defun submit-task (channel function &rest args)
  "Submit a task through `channel' to the kernel stored in `channel'."
  (submit-raw-task (make-channeled-task channel function args)
                   (channel-kernel channel)))

(defun receive-result (channel)
  "Remove a result from `channel'. If nothing is available the call
will block until a result is received."
  (unwrap-result (pop-queue (channel-queue channel))))

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
`*kernel-task-category*' (which has a default value of `:default').

`kill-tasks' rudely interrupts running tasks whose category is `eq' to
`task-category'. The corresponding worker threads are killed and
replaced.

Pending tasks are not affected.

If you don't know what to pass for `task-category' then you should
probably pass `:default', though this may kill more tasks than you
wish. Binding `*kernel-task-category*' around `submit-task' enables
targeted task killing.

If `dry-run' is nil, the function returns the number of tasks killed.

If `dry-run' is non-nil then no tasks are killed. In this case the
return value is the number of tasks that would have been killed if
`dry-run' were nil.

`kill-tasks' is not available in ABCL."
  (when *kernel*
    (with-kernel-slots (workers tasks) *kernel*
      (with-locked-biased-queue tasks
        (let1 victims (map 'vector 
                           #'thread 
                           (remove-if-not (lambda (worker)
                                            (eq (running-category worker)
                                                task-category))
                                          workers))
          (unless dry-run
            (map nil #'destroy-thread victims))
          (length victims))))))

;; TODO: remove sometime
#-abcl
(alias-function emergency-kill-tasks kill-tasks)

(defun kernel-idle-p (kernel)
  (with-kernel-slots (tasks workers) kernel
    (with-locked-biased-queue tasks
      (and (biased-queue-empty-p/no-lock tasks)
           (notany #'running-category workers)))))

(defun wait-for-tasks (channel kernel)
  (loop
     (when (kernel-idle-p kernel)
       (return))
     (let1 *kernel-task-priority* :low
       (submit-task channel (lambda ())))
     (receive-result channel)))

(defun shutdown (channel kernel)
  (wait-for-tasks channel kernel)
  (with-kernel-slots (tasks workers) kernel
    (with-locked-biased-queue tasks
      (repeat (length workers)
        (push-biased-queue/no-lock nil tasks)))))

(defun end-kernel (&key wait)
  "Sets `*kernel*' to nil and ends all workers gracefully.

But hang on -- are you certain you wish to do this? `end-kernel' is an
expensive operation involving heavy locking to detect a finished
state. Creating and destroying threads is also expensive. A kernel is
meant to be your trusted friend for the lifetime of the Lisp process.
Having more than one kernel is fine; simply use `let' to bind a kernel
instance to `*kernel*' when you need it. Use `kill-tasks' to terminate
deadlocked or infinite looping tasks.

If `wait' is nil (the default) then `end-kernel' returns immediately.
Current tasks are waited upon by a separate shutdown manager thread. 

If `wait' is non-nil then `end-kernel' blocks until all tasks are
complete. No shutdown manager thread is created. If you are merely
waiting on tasks then you almost certainly want to use
`receive-result' instead. However there are rare cases where waiting
on a temporary kernel is warranted, for example when benchmarking with
a variety of kernels.

A list of the implementation-defined worker thread objects is
returned. If `wait' is nil then the shutdown manager thread is also
returned as the first element in the list."
  (when *kernel*
    (let ((kernel *kernel*)
          (channel (make-channel)))
      (setf *kernel* nil)
      (labels ((call-shutdown ()
                 (shutdown channel kernel))
               (spawn-shutdown ()
                 (make-thread #'call-shutdown
                              :name "lparallel kernel shutdown manager")))
        (let1 threads (map 'list #'thread (workers kernel))
          (cond (wait
                 (call-shutdown)
                 threads)
                (t
                 (cons (spawn-shutdown) threads))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; timeout
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots timeout ()
  ((canceled-result)
   (thread)
   (lock :initform (make-lock))))

(defun submit-timeout (channel timeout-seconds timeout-result)
  "Effectively equivalent to

  (submit-task channel (lambda () (sleep timeout-seconds) timeout-result))

The difference is that `submit-timeout' does not occupy a worker
thread. A new thread is created for the timeout.

A timeout object is returned, which may be passed to `cancel-timeout'.

If the internal timeout thread is interrupted by means other than
`cancel-timeout' then the channel will receive a `task-killed-error'.
The error is signaled at the point `receive-result' is called."
  (let ((timeout (make-timeout-instance
                  :canceled-result 'not-canceled :thread nil))
        (pushedp nil))
    (with-channel-slots (queue) channel
      (with-timeout-slots (canceled-result thread lock) timeout
        (macrolet ((push-result (form)
                     ;; Ensure that only one result is pushed.
                     ;; 
                     ;; We must check the canceled result inside the
                     ;; lock, so delay evaluation via macrolet.
                     `(with-lock-predicate/wait lock (not pushedp)
                        (push-queue ,form queue)
                        (setf pushedp t))))
          (setf thread (with-thread (:name "lparallel-timeout")
                         (enhanced-unwind-protect
                          :main  (sleep timeout-seconds)
                          :abort (push-result
                                  (if (eq canceled-result 'not-canceled)
                                      (wrap-error 'task-killed-error)
                                      canceled-result)))
                         (push-result timeout-result))))))
    timeout))

(defun cancel-timeout (timeout timeout-result)
  "Attempt to cancel a timeout. If successful, the channel passed to
`submit-timeout' will receive `timeout-result'.

At most one call to `cancel-timeout' will succeed; others will be
ignored. If the timeout has expired on its own then `cancel-timeout'
will have no effect."
  (with-timeout-slots (canceled-result thread lock) timeout
    ;; ensure that only one cancel succeeds
    (with-lock-predicate/wait lock (eq canceled-result 'not-canceled)
      (setf canceled-result timeout-result)
      (destroy-thread thread)))
  nil)
