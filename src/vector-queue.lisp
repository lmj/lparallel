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

(in-package #:lparallel.vector-queue)

(import-now lparallel.thread-util::condition-wait/track-state)

;;;; raw-queue

(deftype raw-queue-count () `(integer 0 ,array-dimension-limit))

(defslots raw-queue ()
  ((data              :reader data            :type simple-array)
   (start :initform 0                         :type raw-queue-count)
   (count :initform 0 :reader raw-queue-count :type raw-queue-count)))

(defun make-raw-queue (capacity)
  (make-raw-queue-instance :data (make-array capacity)))

(defun/type push-raw-queue (value queue) (t raw-queue) null
  (declare #.*normal-optimize*)
  (with-raw-queue-slots (data start count) queue
    (setf (svref data (mod (+ start count) (length data))) value)
    (incf count)
    nil))

(defun/type pop-raw-queue (queue) (raw-queue) (values t boolean)
  (declare #.*normal-optimize*)
  (with-raw-queue-slots (data start count) queue
    (let ((data data))
      (if (plusp count)
          (multiple-value-prog1 (values (svref data start) t)
            (setf (svref data start) nil
                  start (mod (1+ start) (length data)))
            (decf count))
          (values nil nil)))))

(defun/type peek-raw-queue (queue) (raw-queue) (values t boolean)
  (declare #.*normal-optimize*)
  (with-raw-queue-slots (data start count) queue
    (if (plusp count)
        (values (svref data start) t)
        (values nil nil))))

(defun/type/inline raw-queue-empty-p (queue) (raw-queue) t
  (declare #.*normal-optimize*)
  (zerop (raw-queue-count queue)))

(defun/type/inline raw-queue-full-p (queue) (raw-queue) t
  (declare #.*normal-optimize*)
  (eql (raw-queue-count queue) (length (data queue))))

(defun/type/inline raw-queue-capacity (queue) (raw-queue) raw-queue-count
  (declare #.*normal-optimize*)
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

(define-locking-fn push-vector-queue (object queue) (t vector-queue) null lock
  (with-vector-queue-slots (impl lock notify-push notify-pop) queue
    (let ((impl impl))
      (loop (cond ((< (raw-queue-count impl) (raw-queue-capacity impl))
                   (push-raw-queue object impl)
                   (when notify-push
                     (condition-notify-and-yield notify-push))
                   (return))
                  (t
                   (condition-wait
                    (or notify-pop
                        (setf notify-pop (make-condition-variable)))
                    lock)))))))

(define-locking-fn pop-vector-queue (queue) (vector-queue) t lock
  (with-vector-queue-slots (impl lock notify-push notify-pop) queue
    (let ((impl impl))
      (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
              (cond (presentp
                     (when notify-pop
                       (condition-notify-and-yield notify-pop))
                     (return value))
                    (t
                     (condition-wait
                      (or notify-push
                          (setf notify-push (make-condition-variable)))
                      lock))))))))

(defun %try-pop-vector-queue/no-lock (queue timeout)
  (declare #.*normal-optimize*)
  (with-vector-queue-slots (impl lock notify-push notify-pop) queue
    (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
            (cond (presentp
                   (when notify-pop
                     (condition-notify-and-yield notify-pop))
                   (return (values value t)))
                  ((plusp timeout)
                   (condition-wait/track-state notify-push lock timeout))
                  (t
                   (return (values nil nil))))))))

(defun try-pop-vector-queue (queue timeout)
  (declare #.*normal-optimize*)
  (if (plusp timeout)
      (with-lock-held ((lock queue))
        (%try-pop-vector-queue/no-lock queue timeout))
      ;; optimization: don't lock if nothing is there
      (with-vector-queue-slots (impl lock) queue
        (with-lock-predicate/wait lock (not (raw-queue-empty-p impl))
          (return-from try-pop-vector-queue (pop-raw-queue impl)))
        (values nil nil))))

(defun try-pop-vector-queue/no-lock (queue timeout)
  (declare #.*normal-optimize*)
  (if (plusp timeout)
      (%try-pop-vector-queue/no-lock queue timeout)
      (pop-raw-queue (impl queue))))

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
