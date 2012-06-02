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
;;; raw-queue -- raw data structure
;;;

(in-package #:lparallel.raw-queue)

(deftype raw-queue-count () '(integer 0))

#-lparallel.with-vector-queue
(progn
  (deftype raw-queue () '(cons list list))

  (defun/type make-raw-queue
      (&optional initial-capacity) (&optional raw-queue-count) raw-queue
    (declare (ignore initial-capacity))
    (cons nil nil))

  (defun/type push-raw-queue (value queue) (t raw-queue) t
    (declare #.*normal-optimize*)
    (let1 new (cons value nil)
      (if (car queue)
          (setf (cddr queue) new)
          (setf (car  queue) new))
      (setf (cdr queue) new)))

  (defun/type pop-raw-queue (queue) (raw-queue) (values t boolean)
    (declare #.*normal-optimize*)
    (if (car queue)
        (multiple-value-prog1 (values (caar queue) t)
          (unless (setf (car queue) (cdar queue))
            ;; clear lingering ref
            (setf (cdr queue) nil)))
        (values nil nil)))

  (defun/inline raw-queue-count   (queue) (length (the list (car queue))))
  (defun/inline raw-queue-empty-p (queue) (not (car queue)))
  (defun/inline peek-raw-queue    (queue) (values (caar queue)
                                                  (if (car queue) t nil))))

#+lparallel.with-vector-queue
(progn
  (defslots raw-queue ()
    ((data                                      :type array)
     (start :initform 0                         :type raw-queue-count)
     (count :initform 0 :reader raw-queue-count :type raw-queue-count)))

  (defun make-raw-queue (&optional (initial-capacity 2)) 
    (declare #.*normal-optimize*)
    (make-raw-queue-instance
     :data (make-array initial-capacity :adjustable t)))

  (defun/type push-raw-queue (value queue) (t raw-queue) null
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

  (defun/type pop-raw-queue (queue) (raw-queue) (values t boolean)
    (declare #.*normal-optimize*)
    (with-raw-queue-slots (data start count) queue
      (if (plusp count)
          (multiple-value-prog1 (values (aref data start) t)
            (setf (aref data start) nil
                  start (mod (1+ start) (length data)))
            (decf count))
          (values nil nil))))

  (defun/type/inline peek-raw-queue (queue) (raw-queue) (values t boolean)
    (declare #.*normal-optimize*)
    (with-raw-queue-slots (data start count) queue
      (if (plusp count)
          (values (aref data start) t)
          (values nil nil))))

  (defun/type/inline raw-queue-empty-p (queue) (raw-queue) t
    (declare #.*normal-optimize*)
    (zerop (raw-queue-count queue))))
