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

(defpackage #:lparallel.vector-queue
  (:documentation
   "(private) Blocking fixed-capacity queue.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util)
  (:export #:vector-queue
           #:make-vector-queue
           #:push-vector-queue    #:push-vector-queue/no-lock
           #:pop-vector-queue     #:pop-vector-queue/no-lock
           #:peek-vector-queue    #:peek-vector-queue/no-lock
           #:vector-queue-count   #:vector-queue-count/no-lock
           #:vector-queue-empty-p #:vector-queue-empty-p/no-lock
           #:vector-queue-full-p  #:vector-queue-full-p/no-lock
           #:try-pop-vector-queue #:try-pop-vector-queue/no-lock
           #:with-locked-vector-queue
           #:vector-queue-capacity)
  (:import-from #:lparallel.thread-util
                #:define-locking-fn
                #:define-simple-locking-fn
                #:with-countdown
                #:time-remaining)
  (:import-from #:alexandria
                #:array-length))

(in-package #:lparallel.vector-queue)

;;;; raw-queue

(deftype raw-queue-count () 'array-length)

(defslots raw-queue ()
  ((data              :reader data            :type simple-array)
   (start :initform 0                         :type index)
   (count :initform 0 :reader raw-queue-count :type raw-queue-count)))

(defun make-raw-queue (capacity)
  (make-raw-queue-instance :data (make-array capacity)))

(defun/type push-raw-queue (value queue) (t raw-queue) (values)
  (declare #.*full-optimize*)
  (with-raw-queue-slots (data start count) queue
    (setf (svref data (mod (+ start count) (length data))) value)
    (incf count))
  (values))

(defun/type pop-raw-queue (queue) (raw-queue) (values t boolean)
  (declare #.*full-optimize*)
  (with-raw-queue-slots (data start count) queue
    (let ((data data))
      (if (plusp count)
          (multiple-value-prog1 (values (svref data start) t)
            (setf (svref data start) nil
                  start (mod (1+ start) (length data)))
            (decf count))
          (values nil nil)))))

(defun/type peek-raw-queue (queue) (raw-queue) (values t boolean)
  (declare #.*full-optimize*)
  (with-raw-queue-slots (data start count) queue
    (if (plusp count)
        (values (svref data start) t)
        (values nil nil))))

(defun/type/inline raw-queue-empty-p (queue) (raw-queue) t
  (declare #.*full-optimize*)
  (zerop (raw-queue-count queue)))

(defun/type/inline raw-queue-full-p (queue) (raw-queue) t
  (declare #.*full-optimize*)
  (eql (raw-queue-count queue) (length (data queue))))

(defun/type/inline raw-queue-capacity (queue) (raw-queue) raw-queue-count
  (declare #.*full-optimize*)
  (length (data queue)))

;;;; vector-queue

(defslots vector-queue ()
  ((impl :reader impl :type raw-queue)
   (lock :reader lock :initform (make-lock))
   (notify-push :initform nil)
   (notify-pop :initform nil)))

(defun %make-vector-queue (capacity)
  (make-vector-queue-instance :impl (make-raw-queue capacity)))

(defmacro with-locked-vector-queue (queue &body body)
  `(with-lock-held ((lock ,queue))
     ,@body))

(define-locking-fn push-vector-queue (object queue)
    (t vector-queue) (values) lock
  (with-vector-queue-slots (impl lock notify-push notify-pop) queue
    (loop (cond ((< (raw-queue-count impl) (raw-queue-capacity impl))
                 (push-raw-queue object impl)
                 (when notify-push
                   (condition-notify notify-push))
                 (return))
                (t
                 (condition-wait
                  (or notify-pop
                      (setf notify-pop (make-condition-variable)))
                  lock)))))
  (values))

(define-locking-fn pop-vector-queue (queue) (vector-queue) t lock
  (with-vector-queue-slots (impl lock notify-push notify-pop) queue
    (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
            (cond (presentp
                   (when notify-pop
                     (condition-notify notify-pop))
                   (return value))
                  (t
                   (condition-wait
                    (or notify-push
                        (setf notify-push (make-condition-variable)))
                    lock)))))))

(defun %try-pop-vector-queue/no-lock/timeout (queue timeout)
  ;; queue is empty and timeout is positive
  (declare #.*full-optimize*)
  (with-countdown (timeout)
    (with-vector-queue-slots (impl lock notify-push notify-pop) queue
      (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
              (when presentp
                (when notify-pop
                  (condition-notify notify-pop))
                (return (values value t)))
              (let ((time-remaining (time-remaining)))
                (when (or (not (plusp time-remaining))
                          (null (condition-wait
                                 (or notify-push
                                     (setf notify-push
                                           (make-condition-variable)))
                                 lock :timeout time-remaining)))
                  (return (values nil nil)))))))))

(defun try-pop-vector-queue/no-lock/no-timeout (queue)
  (declare #.*full-optimize*)
  (with-vector-queue-slots (impl notify-pop) queue
    (multiple-value-bind (value presentp) (pop-raw-queue impl)
      (cond (presentp
             (when notify-pop
               (condition-notify notify-pop))
             (values value t))
            (t
             (values nil nil))))))

(defun try-pop-vector-queue/no-lock/timeout (queue timeout)
  (declare #.*full-optimize*)
  (with-vector-queue-slots (impl) queue
    (if (raw-queue-empty-p impl)
        (%try-pop-vector-queue/no-lock/timeout queue timeout)
        (try-pop-vector-queue/no-lock/no-timeout queue))))

(defun try-pop-vector-queue (queue timeout)
  (declare #.*full-optimize*)
  (with-vector-queue-slots (impl lock) queue
    (cond ((plusp timeout)
           (with-lock-held (lock)
             (try-pop-vector-queue/no-lock/timeout queue timeout)))
          (t
           ;; optimization: don't lock if nothing is there
           (with-lock-predicate/wait lock (not (raw-queue-empty-p impl))
             (return-from try-pop-vector-queue
               (try-pop-vector-queue/no-lock/no-timeout queue)))
           (values nil nil)))))

(defun try-pop-vector-queue/no-lock (queue timeout)
  (declare #.*full-optimize*)
  (if (plusp timeout)
      (try-pop-vector-queue/no-lock/timeout queue timeout)
      (try-pop-vector-queue/no-lock/no-timeout queue)))

(defmacro define-queue-fn (name arg-types raw return-type)
  `(define-simple-locking-fn ,name (queue) ,arg-types ,return-type lock
     (,raw (impl queue))))

(define-queue-fn vector-queue-count (vector-queue)
  raw-queue-count
  raw-queue-count)

(define-queue-fn vector-queue-capacity (vector-queue)
  raw-queue-capacity
  raw-queue-count)

(define-queue-fn vector-queue-empty-p (vector-queue)
  raw-queue-empty-p
  boolean)

(define-queue-fn vector-queue-full-p (vector-queue)
  raw-queue-full-p
  boolean)

(define-queue-fn peek-vector-queue (vector-queue)
  peek-raw-queue
  (values t boolean))

(defun make-vector-queue (capacity &key initial-contents)
  (let ((queue (%make-vector-queue capacity)))
    (when initial-contents
      (block done
        (flet ((push-elem (elem)
                 (when (vector-queue-full-p/no-lock queue)
                   (return-from done))
                 (push-vector-queue/no-lock elem queue)))
          (declare (dynamic-extent #'push-elem))
          (map nil #'push-elem initial-contents))))
    queue))
