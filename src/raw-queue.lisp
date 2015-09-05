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

(defpackage #:lparallel.raw-queue
  (:documentation
   "(private) Raw queue data structure.")
  (:use #:cl
        #:lparallel.util)
  (:export #:raw-queue
           #:make-raw-queue
           #:push-raw-queue
           #:pop-raw-queue
           #:peek-raw-queue
           #:raw-queue-count
           #:raw-queue-empty-p))

(in-package #:lparallel.raw-queue)

(deftype raw-queue-count () '(integer 0))

(locally (declare #.*full-optimize*)
  (defstruct (raw-queue (:conc-name nil)
                        (:constructor %make-raw-queue (head tail)))
    (head (error "no head") :type list)
    (tail (error "no tail") :type list)))

(defun/inline make-raw-queue (&optional initial-capacity)
  (declare (ignore initial-capacity))
  (%make-raw-queue nil nil))

(defun/type push-raw-queue (value queue) (t raw-queue) t
  (declare #.*full-optimize*)
  (let ((new (cons value nil)))
    (if (head queue)
        (setf (cdr (tail queue)) new)
        (setf (head queue) new))
    (setf (tail queue) new)))

(defun/type pop-raw-queue (queue) (raw-queue) (values t boolean)
  (declare #.*full-optimize*)
  (let ((node (head queue)))
    (if node
        (multiple-value-prog1 (values (car node) t)
          (when (null (setf (head queue) (cdr node)))
            (setf (tail queue) nil))
          ;; clear node for conservative gcs
          (setf (car node) nil
                (cdr node) nil))
        (values nil nil))))

(defun/inline raw-queue-count   (queue) (length (the list (head queue))))
(defun/inline raw-queue-empty-p (queue) (not (head queue)))
(defun/inline peek-raw-queue    (queue) (let ((node (head queue)))
                                          (values (car node)
                                                  (if node t nil))))
