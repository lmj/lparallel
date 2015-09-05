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

;;;
;;; Similar to a priority queue but with only two tiers. O(1)
;;; insertion and removal.
;;;

(defpackage #:lparallel.biased-queue
  (:documentation
   "(private) Blocking two-tiered priority queue.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.raw-queue)
  (:export #:biased-queue
           #:make-biased-queue
           #:push-biased-queue     #:push-biased-queue/no-lock
           #:push-biased-queue/low #:push-biased-queue/low/no-lock
           #:pop-biased-queue      #:pop-biased-queue/no-lock
           #:peek-biased-queue     #:peek-biased-queue/no-lock
           #:biased-queue-empty-p  #:biased-queue-empty-p/no-lock
           #:try-pop-biased-queue  #:try-pop-biased-queue/no-lock
           #:pop-biased-queue      #:pop-biased-queue/no-lock
           #:biased-queue-count    #:biased-queue-count/no-lock
           #:with-locked-biased-queue)
  (:import-from #:lparallel.thread-util
                #:define-locking-fn
                #:define-simple-locking-fn))

(in-package #:lparallel.biased-queue)

(defslots biased-queue ()
  ((lock :reader lock :initform (make-lock))
   (cvar :reader cvar :initform (make-condition-variable))
   (high :reader high :type raw-queue)
   (low  :reader low  :type raw-queue)))

(defun make-biased-queue (&optional (size 1))
  (make-biased-queue-instance :high (make-raw-queue size)
                              :low  (make-raw-queue)))

(defmacro define-push-fn (name slot)
  `(define-simple-locking-fn ,name (object queue) (t biased-queue) (values) lock
     (push-raw-queue object (,slot queue))
     (condition-notify (cvar queue))
     (values)))

(define-push-fn push-biased-queue     high)
(define-push-fn push-biased-queue/low low)

(defmacro define-high-low-fn (name operation)
  `(define-locking-fn ,name (queue) (biased-queue) (values t boolean) lock
     (with-biased-queue-slots (high low) queue
       (multiple-value-bind (object presentp) (,operation high)
         (if presentp
             (values object t)
             (,operation low))))))

(define-high-low-fn try-pop-biased-queue pop-raw-queue)
(define-high-low-fn peek-biased-queue peek-raw-queue)

(define-locking-fn pop-biased-queue (queue) (biased-queue) t lock
  (with-biased-queue-slots (lock cvar) queue
    (loop (multiple-value-bind (value presentp)
              (try-pop-biased-queue/no-lock queue)
            (if presentp
                (return value)
                (condition-wait cvar lock))))))

(define-simple-locking-fn
    biased-queue-empty-p (queue) (biased-queue) boolean lock
  (and (raw-queue-empty-p (high queue))
       (raw-queue-empty-p (low queue))))

(define-simple-locking-fn
    biased-queue-count (queue) (biased-queue) (integer 0) lock
  (+ (raw-queue-count (high queue))
     (raw-queue-count (low queue))))

(defmacro with-locked-biased-queue (queue &body body)
  `(with-lock-held ((lock ,queue))
     ,@body))
