;;; Copyright (c) 2014, James M. Lawrence. All rights reserved.
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

(defpackage #:lparallel.kernel
  (:documentation
   "Encompasses the scheduling and execution of parallel tasks using a
   pool of worker threads. All parallelism in lparallel is done on top
   of the kernel.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.queue
        #-lparallel.with-stealing-scheduler #:lparallel.biased-queue
        #+lparallel.with-stealing-scheduler #:lparallel.counter
        #+lparallel.with-stealing-scheduler #:lparallel.spin-queue)
  (:export #:make-kernel
           #:check-kernel
           #:end-kernel
           #:kernel-worker-count
           #:kernel-worker-index
           #:kernel-bindings
           #:kernel-name
           #:kernel-context)
  (:export #:make-channel
           #:submit-task
           #:broadcast-task
           #:submit-timeout
           #:cancel-timeout
           #:receive-result
           #:try-receive-result
           #:do-fast-receives
           #:kill-tasks
           #:task-handler-bind
           #:task-categories-running
           #:invoke-transfer-error)
  (:export #:*kernel*
           #:*kernel-spin-count*
           #:*task-category*
           #:*task-priority*
           #:*debug-tasks-p*)
  (:export #:kernel
           #:channel
           #:transfer-error
           #:no-kernel-error
           #:kernel-creation-error
           #:task-killed-error)
  (:import-from #:alexandria
                #:simple-style-warning))

(in-package #:lparallel.kernel)
