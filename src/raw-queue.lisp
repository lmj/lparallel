;;; Copyright (c) 2011, James M. Lawrence. All rights reserved.
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
;;; raw-queue -- raw data structure
;;;

(in-package #:lparallel.raw-queue)

(deftype raw-queue-count () 'fixnum)

(defslots raw-queue ()
  ((data                                      :type array)
   (start :initform 0                         :type fixnum)
   (count :initform 0 :reader raw-queue-count :type raw-queue-count))
  (:documentation
   "A simple FIFO queue."))

(defun make-raw-queue (&optional (initial-capacity 2)) 
  "Make a FIFO queue."
  (declare #.*normal-optimize*)
  (make-raw-queue-instance :data (make-array initial-capacity :adjustable t)))

(defun/ftype push-raw-queue (value queue) (function (t raw-queue) null)
  "Push `value' onto the back of `queue'."
  (declare #.*normal-optimize*)
  (with-raw-queue-slots (data start count) queue
    (let1 size (length data)
      (cond ((eql count size)
             (adjust-array data (if (zerop size) 1 (* 2 size)))
             (replace data data :start1 size :start2 0 :end2 start)
             (fill data nil :start 0 :end start)
             (setf (aref data (mod (+ start count) (length data))) value))
            (t
             (setf (aref data (mod (+ start count) size)) value))))
    (incf count)
    nil))

(defun/ftype pop-raw-queue (queue) (function (raw-queue) (values t boolean))
  "If `queue' is non-empty, pop an element from the front of `queue' and
return (values elem t). 

Otherwise if `queue' empty, return (values nil nil)."
  (declare #.*normal-optimize*)
  (with-raw-queue-slots (data start count) queue
    (if (plusp count)
        (multiple-value-prog1 (values (aref data start) t)
          (setf (aref data start) nil
                start (mod (1+ start) (length data)))
          (decf count))
        (values nil nil))))

(defun/inline peek-raw-queue (queue)
  "If `queue' is non-empty, return (values elem t) where `elem' is the
frontmost element of `queue'.

Otherwise if `queue' is empty, return (values nil nil)."
  (declare #.*normal-optimize*)
  (with-raw-queue-slots (data start count) queue
    (if (plusp count)
        (values (aref data start) t)
        (values nil nil))))

(defun/inline raw-queue-empty-p (queue)
  "If queue is empty, return true. Otherwise return false."
  (declare #.*normal-optimize*)
  (zerop (raw-queue-count queue)))
