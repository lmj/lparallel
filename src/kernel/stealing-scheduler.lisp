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

;;;; util

(defmacro define-mod-inc-dec (name op op-result-type)
  `(defmacro ,name (k n)
     `(the index (mod (the ,',op-result-type (,',op (the index ,k)))
                      (the index ,n)))))

(define-mod-inc-dec mod-inc 1+ index)
(define-mod-inc-dec mod-dec 1- fixnum)

(defmacro define-mod-incf-decf (name op)
  `(defmacro ,name (place n)
     `(the index (setf ,place (,',op ,place ,n)))))

(define-mod-incf-decf mod-incf mod-inc)
(define-mod-incf-decf mod-decf mod-dec)

(defmacro with-pop-success (var queue &body body)
  (with-gensyms (presentp)
    `(multiple-value-bind (,var ,presentp) (pop-spin-queue ,queue)
       (when ,presentp
         ,@body))))

(defmacro repeat/fixnum (count &body body)
  (with-gensyms (left)
    `(let1 ,left (the fixnum ,count)
       (declare (type fixnum ,left))
       (loop
          (when (zerop ,left)
            (return))
          (decf ,left)
          ,@body))))

(defmacro do-indexes ((var size-form start from-start-p) &body body)
  (with-gensyms (size)
    `(let ((,var (the index ,start))
           (,size (the index ,size-form)))
       (declare (type index ,var ,size))
       (repeat/fixnum ,size
         ,(let1 next `(mod-incf ,var ,size)
            (if from-start-p
                `(progn ,@body ,next)
                `(progn ,next ,@body)))))))

;;;; scheduler

(defun make-scheduler (workers spin-count)
  (make-scheduler-instance :workers workers :spin-count spin-count))

(defun/type/inline push-to-random-worker (task scheduler) (task scheduler) t
  ;; Decrease random-index without caring about simultaneous changes.
  ;; The actual value of random-index does not matter as long as it
  ;; remains somewhat well-distributed.
  (declare #.*full-optimize*)
  (with-scheduler-slots (workers random-index) scheduler
    (push-spin-queue
     task (tasks (svref workers (mod-decf random-index (length workers)))))))

(defun/type maybe-wake-a-worker (scheduler) (scheduler) t
  (declare #.*full-optimize*)
  (with-scheduler-slots (wait-lock wait-cvar wait-count notify-count) scheduler
    (with-lock-predicate/wait wait-lock (plusp (counter-value wait-count))
      (incf notify-count)
      (condition-notify-and-yield wait-cvar))))

(defun/type schedule-task (scheduler task priority) (scheduler
                                                     (or task null) t) t
  (declare #.*full-optimize*)
  (ccase priority
    (:low     (with-scheduler-slots (low-priority-tasks) scheduler
                (push-spin-queue task low-priority-tasks)))
    (:default (push-to-random-worker task scheduler)))
  (maybe-wake-a-worker scheduler))

(defmacro do-workers ((worker-var workers start-index from-start-p)
                      &body body)
  (with-gensyms (worker-index)
    `(do-indexes (,worker-index
                  (length (the simple-vector ,workers))
                  ,start-index
                  ,from-start-p)
       (let1 ,worker-var (svref (the simple-vector ,workers) ,worker-index)
         (declare (type worker ,worker-var))
         ,@body))))

(defun/type next-task (scheduler worker) (scheduler worker) (or task null)
  (declare #.*full-optimize*)
  (labels ((try-pop (queue)
             (declare (type spin-queue queue))
             (with-pop-success task queue
               (return-from next-task task)))
           (find-a-task ()
             (try-pop (tasks worker))
             (with-scheduler-slots (workers) scheduler
               (do-workers (worker workers (worker-index worker) nil)
                 (try-pop (tasks worker)))))
           (maybe-sleep ()
             (with-scheduler-slots (wait-cvar wait-lock wait-count
                                    notify-count low-priority-tasks) scheduler
               (unwind-protect/ext
                :prepare (inc-counter wait-count)
                :main    (with-lock-held (wait-lock)
                           (try-pop (tasks worker))
                           (try-pop low-priority-tasks)
                           (loop
                              :until   (plusp notify-count)
                              :do      (condition-wait wait-cvar wait-lock)
                              :finally (decf notify-count)))
                :cleanup (dec-counter wait-count)))))
    (declare (dynamic-extent #'try-pop #'find-a-task #'maybe-sleep))
    (with-scheduler-slots (spin-count) scheduler
      (loop
         (find-a-task)
         (repeat/fixnum spin-count
           (find-a-task))
         (maybe-sleep)))))

(defun/type steal-task (scheduler) (scheduler) (or task null)
  (declare #.*full-optimize*)
  (with-scheduler-slots (workers random-index low-priority-tasks) scheduler
    ;; Start with the worker that has the most recently submitted task
    ;; (approximately) and advance rightward.
    (do-workers (worker workers random-index t)
      (with-pop-success task (tasks worker)
        (if task
            (return-from steal-task task)
            ;; don't steal nil, the end condition flag
            (push-spin-queue task low-priority-tasks)))))
  nil)
