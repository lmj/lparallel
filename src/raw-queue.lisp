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

(deftype raw-queue () 'cons)
(deftype raw-queue-count () '(integer 0 *))

(defun/inline make-raw-queue (&optional initial-capacity)
  (declare (ignore initial-capacity))
  (cons nil nil))

(defun/ftype push-raw-queue (value z) (function (t raw-queue) t)
  (declare #.*normal-optimize*)
  (let1 new (cons value nil)
    (if (car z)
        (setf (cddr z) new)
        (setf (car  z) new))
    (setf (cdr z) new)))

(defun/ftype pop-raw-queue (z) (function (raw-queue) (values t boolean))
  (declare #.*normal-optimize*)
  (if (car z)
      (multiple-value-prog1 (values (caar z) t)
        (unless (setf (car z) (cdar z))
          ;; clear lingering ref
          (setf (cdr z) nil)))
      (values nil nil)))
  
(defun/inline raw-queue-count   (z) (length (car z)))
(defun/inline raw-queue-empty-p (z) (not (car z)))
(defun/inline peek-raw-queue    (z) (values (caar z) (if (car z) t nil)))
