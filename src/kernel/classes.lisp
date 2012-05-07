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
   (name     :type string)))

(defslots worker-notifications ()
  ((handshake/from-worker :initform (make-queue))
   (handshake/to-worker   :initform (make-queue))
   (exit-notification     :initform (make-queue))))

(defslots worker (worker-notifications)
  ((thread            :reader thread)
   (running-category  :reader running-category :initform nil)
   (index             :reader worker-index     :type fixnum)
   #+lparallel.with-stealing-scheduler
   (tasks             :reader tasks            :type spin-queue)))

#+lparallel.with-stealing-scheduler
(defslots scheduler ()
  ((workers                                        :type simple-vector)
   (wait-cvar          :initform (make-condition-variable))
   (wait-lock          :initform (make-lock))
   (wait-count         :initform 0                 :type fixnum)
   (notify-count       :initform 0)
   (spin-count)
   (low-priority-tasks :initform (make-spin-queue) :type spin-queue)))

#-lparallel.with-stealing-scheduler
(progn
  (deftype scheduler () 'biased-queue)
  (defun tasks (scheduler) (declare (ignore scheduler))))

(locally (declare #.*full-optimize*)
  (defslots optimizer ()
    ((optimizer-flag :reader optimizer-flag :initform t :type boolean)
     (optimizer-data :reader optimizer-data))))

;;; optimizer is included (as opposed to being a member) because
;;; optimizer-flag is a critical inline function
(defslots kernel (optimizer)
  ((scheduler       :reader scheduler :type scheduler)
   (workers         :reader workers   :type simple-vector)
   (workers-lock)
   (worker-info                       :type worker-info)))

(defslots channel ()
  ((queue  :reader channel-queue  :type queue)
   (kernel :reader channel-kernel :type kernel)))

(defpair task ()
  ((fn       :reader task-fn :type function)
   (category :reader task-category)))
