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

(define-locking-fn push-cons-queue (object queue) (t cons-queue) null lock
  (with-cons-queue-slots (impl cvar) queue
    (push-raw-queue object impl)
    (when cvar
      (condition-notify-and-yield cvar)))
  nil)

(define-locking-fn pop-cons-queue (queue) (cons-queue) t lock
  (with-cons-queue-slots (impl lock cvar) queue
    (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
            (if presentp
                (return value)
                (condition-wait (or cvar (setf cvar (make-condition-variable)))
                                lock))))))

(defun/type try-pop-cons-queue (queue) (cons-queue) (values t boolean)
  (declare #.*normal-optimize*)
  (with-cons-queue-slots (impl lock) queue
    (with-lock-predicate/wait lock (not (raw-queue-empty-p impl))
      (return-from try-pop-cons-queue (pop-raw-queue impl)))
    (values nil nil)))

(defun/type/inline try-pop-cons-queue/no-lock (queue)
    (cons-queue) (values t boolean)
  (declare #.*normal-optimize*)
  (pop-raw-queue (impl queue)))

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
