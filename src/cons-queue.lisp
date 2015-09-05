;;; Copyright (c) 2011-2013, James M. Lawrence. All rights reserved.
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

(defpackage #:lparallel.cons-queue
  (:documentation
   "(private) Blocking infinite-capacity queue.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.raw-queue)
  (:export #:cons-queue
           #:make-cons-queue
           #:push-cons-queue    #:push-cons-queue/no-lock
           #:pop-cons-queue     #:pop-cons-queue/no-lock
           #:peek-cons-queue    #:peek-cons-queue/no-lock
           #:cons-queue-count   #:cons-queue-count/no-lock
           #:cons-queue-empty-p #:cons-queue-empty-p/no-lock
           #:try-pop-cons-queue #:try-pop-cons-queue/no-lock
           #:with-locked-cons-queue)
  (:import-from #:lparallel.thread-util
                #:define-locking-fn
                #:define-simple-locking-fn
                #:with-countdown
                #:time-remaining))

(in-package #:lparallel.cons-queue)

(defslots cons-queue ()
  ((impl :reader impl                      :type raw-queue)
   (lock :reader lock :initform (make-lock))
   (cvar              :initform nil)))

(defun %make-cons-queue ()
  (make-cons-queue-instance :impl (make-raw-queue)))

(defmacro with-locked-cons-queue (queue &body body)
  `(with-lock-held ((lock ,queue))
     ,@body))

(define-locking-fn push-cons-queue (object queue) (t cons-queue) (values) lock
  (with-cons-queue-slots (impl cvar) queue
    (push-raw-queue object impl)
    (when cvar
      (condition-notify cvar)))
  (values))

(define-locking-fn pop-cons-queue (queue) (cons-queue) t lock
  (with-cons-queue-slots (impl lock cvar) queue
    (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
            (if presentp
                (return value)
                (condition-wait (or cvar (setf cvar (make-condition-variable)))
                                lock))))))

(defun %try-pop-cons-queue/no-lock/timeout (queue timeout)
  ;; queue is empty and timeout is positive
  (declare #.*full-optimize*)
  (with-countdown (timeout)
    (with-cons-queue-slots (impl lock cvar) queue
      (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
              (when presentp
                (return (values value t)))
              (let ((time-remaining (time-remaining)))
                (when (or (not (plusp time-remaining))
                          (null (condition-wait
                                 (or cvar (setf cvar (make-condition-variable)))
                                 lock :timeout time-remaining)))
                  (return (values nil nil)))))))))

(defun try-pop-cons-queue/no-lock/timeout (queue timeout)
  (declare #.*full-optimize*)
  (with-cons-queue-slots (impl) queue
    (if (raw-queue-empty-p impl)
        (%try-pop-cons-queue/no-lock/timeout queue timeout)
        (pop-raw-queue impl))))

(defun try-pop-cons-queue (queue timeout)
  (declare #.*full-optimize*)
  (with-cons-queue-slots (impl lock) queue
    (cond ((plusp timeout)
           (with-lock-held (lock)
             (try-pop-cons-queue/no-lock/timeout queue timeout)))
          (t
           ;; optimization: don't lock if nothing is there
           (with-lock-predicate/wait lock (not (raw-queue-empty-p impl))
             (return-from try-pop-cons-queue (pop-raw-queue impl)))
           (values nil nil)))))

(defun try-pop-cons-queue/no-lock (queue timeout)
  (declare #.*full-optimize*)
  (if (plusp timeout)
      (try-pop-cons-queue/no-lock/timeout queue timeout)
      (pop-raw-queue (impl queue))))

(defmacro define-queue-fn (name arg-types raw return-type)
  `(define-simple-locking-fn ,name (queue) ,arg-types ,return-type lock
     (,raw (impl queue))))

(define-queue-fn cons-queue-count (cons-queue)
  raw-queue-count
  raw-queue-count)

(define-queue-fn cons-queue-empty-p (cons-queue)
  raw-queue-empty-p
  boolean)

(define-queue-fn peek-cons-queue (cons-queue)
  peek-raw-queue
  (values t boolean))

(defun make-cons-queue (&key initial-contents)
  (let ((queue (%make-cons-queue)))
    (when initial-contents
      (flet ((push-elem (elem)
               (push-cons-queue/no-lock elem queue)))
        (declare (dynamic-extent #'push-elem))
        (map nil #'push-elem initial-contents)))
    queue))
