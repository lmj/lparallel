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

;;; The limiter, if in use, places a limit on the number of queued
;;; tasks. This must be a struct for CAS. The `limiter-accept-task-p'
;;; flag must be fast/inlined in order to be useful, which is why the
;;; kernel subclasses directly from this."
#-lparallel.with-debug
(locally (declare #.*full-optimize*)
  (defstruct (limiter (:conc-name nil))
    (limiter-accept-task-p (error "no init") :type boolean)
    (limiter-lock (error "no init"))
    (limiter-count (error "no init") :type fixnum)))

;;; Debug version of limiter can't be a struct since in this case
;;; `defslots' expands to `defclass'.
#+lparallel.with-debug
(defclass limiter ()
  ((limiter-accept-task-p :accessor limiter-accept-task-p
                          :initarg :limiter-accept-task-p
                          :type boolean)
   (limiter-lock :accessor limiter-lock
                 :initarg :limiter-lock)
   (limiter-count :accessor limiter-count
                  :initarg :limiter-count
                  :type fixnum)))

(locally (declare #.*full-optimize*)
  (defslots kernel (limiter)
    ((scheduler    :reader scheduler    :type scheduler)
     (workers      :reader workers      :type simple-vector)
     (workers-lock)
     (worker-info                       :type worker-info)
     (use-caller-p :reader use-caller-p :type boolean)
     (alivep       :reader alivep       :type boolean))
    (:documentation
     "The kernel encompasses the scheduling and execution of parallel
     tasks using a pool of worker threads. All parallelism in lparallel
     is done on top of the kernel.")))

(defslots channel ()
  ((queue  :reader channel-queue  :type queue)
   (kernel :reader channel-kernel :type kernel))
  (:documentation
   "A task is submitted to the kernel using a channel. A channel
   always points to the same kernel, which is the value of `*kernel*'
   when the channel is created."))

#-lparallel.without-task-categories
(locally (declare #.*full-optimize*)
  (defpair task ()
    ((fn       :reader task-fn :type function)
     (category :reader task-category))
    (:documentation
     "A task consists of a function and a category. See `kill-tasks' for
     and explanation of task categories.")))

#+lparallel.without-task-categories
(progn
  (deftype task () 'function)
  (defmacro make-task (fn) fn)
  (defmacro task-fn (x) x))
