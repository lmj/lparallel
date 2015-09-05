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
    `(let ((,left (the fixnum ,count)))
       (declare (type fixnum ,left))
       (loop
          (when (zerop ,left)
            (return (values)))
          (decf ,left)
          ,@body))))

(defmacro do-indexes ((index-var size home-index from-home-index-p) &body body)
  ;; size is positive
  (with-gensyms (size-var home-index-var)
    `(let ((,index-var (the index ,home-index))
           (,size-var (the index ,size))
           (,home-index-var (the index ,home-index)))
       (declare (type index ,index-var ,size-var ,home-index-var))
       (loop
          ,(let ((next `(mod-incf ,index-var ,size-var)))
             (if from-home-index-p
                 `(progn ,@body ,next)
                 `(progn ,next ,@body)))
          (when (= ,index-var ,home-index-var)
            (return (values)))))))

;;;; scheduler

(defun make-scheduler (workers spin-count)
  (make-scheduler-instance :workers workers :spin-count spin-count))

(defun/type/inline push-to-random-worker (task scheduler)
    (task scheduler) (values)
  ;; Decrease random-index without caring about simultaneous changes.
  ;; The actual value of random-index does not matter as long as it
  ;; remains somewhat well-distributed.
  (declare #.*full-optimize*)
  (with-scheduler-slots (workers random-index) scheduler
    (push-spin-queue
     task (tasks (svref workers (mod-decf random-index (length workers))))))
  (values))

(defun/type maybe-wake-a-worker (scheduler) (scheduler) (values)
  (declare #.*full-optimize*)
  (with-scheduler-slots (wait-lock wait-cvar wait-count notify-count) scheduler
    (with-lock-predicate/wait wait-lock (plusp (counter-value wait-count))
      (incf notify-count)
      (condition-notify wait-cvar)))
  (values))

(defun/type schedule-task (scheduler task priority)
    (scheduler (or task null) t) (values)
  (declare #.*full-optimize*)
  (ccase priority
    (:low     (with-scheduler-slots (low-priority-tasks) scheduler
                (push-spin-queue task low-priority-tasks)))
    (:default (push-to-random-worker task scheduler)))
  (maybe-wake-a-worker scheduler)
  (values))

(defmacro do-workers ((worker-var workers home-index from-home-index-p)
                      &body body)
  (with-gensyms (workers-var index-var)
    `(let ((,workers-var ,workers))
       (declare (type simple-vector ,workers-var))
       (do-indexes (,index-var
                    (length (the simple-vector ,workers-var))
                    ,home-index
                    ,from-home-index-p)
         (let ((,worker-var (svref (the simple-vector ,workers-var)
                                   ,index-var)))
           (declare (type worker ,worker-var))
           ,@body)))))

(defun/type next-task (scheduler worker) (scheduler worker) (or task null)
  (declare #.*full-optimize*)
  (labels ((try-pop (queue)
             (declare (type spin-queue queue))
             (with-pop-success task queue
               (return-from next-task task))
             (values))
           (try-pop-all ()
             (with-scheduler-slots (workers) scheduler
               (do-workers (worker workers (worker-index worker) nil)
                 (try-pop (tasks worker))))
             (values))
           (maybe-sleep ()
             (with-scheduler-slots (wait-cvar wait-lock wait-count
                                    notify-count low-priority-tasks) scheduler
               (unwind-protect/ext
                :prepare (inc-counter wait-count)
                :main    (with-lock-held (wait-lock)
                           (try-pop (tasks worker))
                           (try-pop low-priority-tasks)
                           (loop until (plusp notify-count)
                                 do (condition-wait wait-cvar wait-lock)
                                 finally (decf notify-count)))
                :cleanup (dec-counter wait-count)))
             (values)))
    (declare (dynamic-extent #'try-pop #'try-pop-all #'maybe-sleep))
    (with-scheduler-slots (spin-count) scheduler
      (loop
         (try-pop (tasks worker))
         (try-pop-all)
         (repeat/fixnum spin-count
           (try-pop-all))
         (maybe-sleep)))))

(defun/type steal-task (scheduler) (scheduler) (or task null)
  (declare #.*full-optimize*)
  (with-scheduler-slots (workers random-index low-priority-tasks) scheduler
    (let ((low-priority-tasks low-priority-tasks))
      (flet ((try-pop (tasks)
               (declare (type spin-queue tasks low-priority-tasks))
               (with-pop-success task tasks
                 (when task
                   (return-from steal-task task))
                 ;; don't steal nil, the end condition flag
                 (push-spin-queue task low-priority-tasks))
               (values)))
        (declare (dynamic-extent #'try-pop))
        ;; Start with the worker that has the most recently submitted
        ;; task (approximately) and advance rightward.
        (do-workers (worker workers random-index t)
          (try-pop (tasks worker)))
        (try-pop low-priority-tasks))))
  nil)
