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

(defmacro inc-mod (place n)
  `(setf ,place (the fixnum (mod (the fixnum (1+ (the fixnum ,place)))
                                 (the fixnum ,n)))))

(defun/type/inline random-fixnum (n) (fixnum) fixnum
  (declare #.*full-optimize*)
  (random (the fixnum n)))

(defmacro with-pop-success (var queue &body body)
  (with-gensyms (presentp)
    `(multiple-value-bind (,var ,presentp) (pop-spin-queue ,queue)
       (when ,presentp
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; scheduler
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun/type make-scheduler (workers spin-count)
    (simple-vector (integer 0)) scheduler
  (make-scheduler-instance :workers workers :spin-count spin-count))

(defun/type/inline push-to-random-worker (task workers) (task simple-vector) t
  (declare #.*normal-optimize*)
  (push-spin-queue
   task
   (tasks (svref workers (random-fixnum (length workers))))))

(defun/type maybe-wake-a-worker (scheduler) (scheduler) t
  (declare #.*normal-optimize*)
  (with-scheduler-slots (wait-lock wait-cvar wait-count notify-count) scheduler
    (with-lock-held (wait-lock)
      (when (plusp wait-count)
        (incf notify-count)
        (condition-notify-and-yield wait-cvar)))))

(defun/type schedule-task (scheduler task priority) (scheduler
                                                     (or task null) t) t
  (declare #.*normal-optimize*)
  (with-scheduler-slots (workers low-priority-tasks) scheduler
    (ccase priority
      (:low     (push-spin-queue task low-priority-tasks))
      (:default (push-to-random-worker task workers))))
  (maybe-wake-a-worker scheduler))

(defmacro/once do-workers ((worker-var &once worker-index scheduler) &body body)
  "Loop through all workers, starting on the right of worker-index."
  (with-gensyms (workers worker-count victim)
    `(locally (declare #.*full-optimize*)
       (with-scheduler-slots ((,workers workers)) ,scheduler
         (let ((,worker-count (the fixnum
                                (length (the simple-vector ,workers))))
               (,victim       (the fixnum ,worker-index)))
           (declare (fixnum ,worker-count ,victim))
           (repeat (the fixnum ,worker-count)
             (inc-mod ,victim ,worker-count)
             (let1 ,worker-var (svref (the simple-vector ,workers)
                                      ,victim)
               (declare (type worker ,worker-var))
               ,@body)))))))

(defun/type next-task (scheduler worker) (scheduler worker) (or task null)
  (declare #.*normal-optimize*)
  (labels ((try-pop (queue)
             (declare (type spin-queue queue))
             (with-pop-success task queue
               (return-from next-task task)))
           (find-a-task ()
             (try-pop (tasks worker))
             (do-workers (worker (worker-index worker) scheduler)
               (try-pop (tasks worker))))
           (maybe-sleep ()
             (with-scheduler-slots (wait-cvar wait-lock wait-count
                                    notify-count low-priority-tasks) scheduler
               (with-lock-held (wait-lock)
                 (try-pop (tasks worker))
                 (try-pop low-priority-tasks)
                 (if (plusp notify-count)
                     (decf notify-count)  ; steal a notification
                     (unwind-protect/ext
                      :prepare (incf wait-count)
                      :main    (loop
                                  :do      (condition-wait wait-cvar wait-lock)
                                  :until   (plusp notify-count)
                                  :finally (decf notify-count))
                      :cleanup (decf wait-count)))))))
    (declare (dynamic-extent #'try-pop #'find-a-task #'maybe-sleep))
    (with-scheduler-slots (spin-count) scheduler
      (loop
         (find-a-task)
         (repeat spin-count
           (find-a-task))
         (maybe-sleep)))))

(defun/type steal-task (scheduler) (scheduler) (or task null)
  (declare #.*full-optimize*)
  (with-scheduler-slots (workers low-priority-tasks) scheduler
    (do-workers (worker (random-fixnum (length workers)) scheduler)
      (with-pop-success task (tasks worker)
        (if task
            (return-from steal-task task)
            ;; don't steal nil, the end condition flag
            (push-spin-queue task low-priority-tasks)))))
  nil)

(defun/type scheduler-empty-p (scheduler) (scheduler) boolean
  (with-scheduler-slots (workers) scheduler
    (every (lambda (worker) (spin-queue-empty-p (tasks worker)))
           workers)))
