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

(defslots worker-info ()
  ((bindings :type list)
   (context  :type function)
   (name     :type string))
  (:documentation
   "Information common to all workers. See `make-kernel'."))

(defslots worker-notifications ()
  ((handshake/from-worker :initform (make-queue))
   (handshake/to-worker   :initform (make-queue))
   (exit-notification     :initform (make-queue)))
  (:documentation
   "Communication with workers. A handshake takes place when a worker
   is created in order to verify its existence and to ensure all data
   is initialized. A worker sends a notification just before it exits."))

(defslots worker (worker-notifications)
  ((thread            :reader thread)
   (running-category  :reader running-category :initform nil)
   (index             :reader worker-index     :type index)
   #+lparallel.with-stealing-scheduler
   (tasks             :reader tasks            :type spin-queue))
  (:documentation
   "A worker represents a thread dedicated to executing tasks. See
   `kill-tasks' for an explanation of `running-category'. `index' is
   the location of the worker in the kernel's vector of workers. When
   the stealing scheduler is enabled, each worker has its own lockless
   task queue."))

#+lparallel.with-stealing-scheduler
(defslots scheduler ()
  ((workers                                        :type simple-vector)
   (wait-cvar          :initform (make-condition-variable))
   (wait-lock          :initform (make-lock))
   (wait-count         :initform (make-counter)    :type counter)
   (notify-count       :initform 0                 :type (integer 0))
   (spin-count                                     :type index)
   (random-index       :initform 0                 :type index)
   (low-priority-tasks :initform (make-spin-queue) :type spin-queue))
  (:documentation
   "A scheduler is responsible for storing tasks and finding the next
   task to execute. A task may also be stolen from the scheduler.

   `workers' -- vector of workers; kernel has the same reference.

   `wait-cvar', `wait-lock', `wait-count', `notify-count' -- these
   coordinate waking/sleeping of workers.

   `spin-count' -- see `make-kernel'.

   `random-index' -- some random index to the vector of workers.

   `low-priority-tasks' -- tasks submitted when `*task-priority*' is `:low'."))

#-lparallel.with-stealing-scheduler
(progn
  ;;; The central queue scheduler. All tasks are submitted to a single
  ;;; queue and all workers pull from the same.
  (deftype scheduler () 'biased-queue)
  (defun tasks (scheduler) (declare (ignore scheduler))))

(locally (declare #.*full-optimize*)
  (defslots optimizer ()
    ((optimizer-flag :reader optimizer-flag :initform t :type boolean)
     (optimizer-data :reader optimizer-data))
    (:documentation
     "Optimization data to be used by a plugin. The `optimizer-flag'
     flag must be inlined in order to be useful, which is why `kernel'
     subclasses directly from this.")))

(defslots kernel (optimizer)
  ((scheduler       :reader scheduler :type scheduler)
   (workers         :reader workers   :type simple-vector)
   (workers-lock)
   (worker-info                       :type worker-info))
  (:documentation
   "`scheduler' -- a scheduler instance.

   `workers' -- vector of workers.

   `workers-lock' -- lock for modification of `workers' vector; used
   when a worker is killed or replaced.

   `worker-info' -- Information common to all workers."))

(defslots channel ()
  ((queue  :reader channel-queue  :type queue)
   (kernel :reader channel-kernel :type kernel))
  (:documentation
   "A task is submitted to the kernel using a channel. When the task
   is complete, the result is pushed to `queue'. A channel always
   points to the same kernel, which is the value of `*kernel*' when
   the channel is created."))

(defpair task ()
  ((fn       :reader task-fn :type function)
   (category :reader task-category))
  (:documentation
   "A task consists of a function and a category. See `kill-tasks' for
   and explanation of task categories."))
