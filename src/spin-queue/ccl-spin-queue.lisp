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

(in-package #:lparallel.spin-queue)

(defconstant +dummy+ 'dummy)

(declaim (inline make-node))
(defstruct (node (:constructor make-node (car cdr)))
  car
  cdr)

(defstruct (spin-queue (:constructor %make-spin-queue (head tail)))
  head
  tail)

(defun make-spin-queue ()
  (let ((dummy (make-node +dummy+ nil)))
    (%make-spin-queue dummy dummy)))

(defun/type push-spin-queue (value queue) (t spin-queue) null
  (declare #.*normal-optimize*)
  (let ((new (make-node value nil))
        (tail (spin-queue-tail queue))
        next)
    (loop
       (loop
          :do (setf next (node-cdr tail))
          :while next
          :do (setf tail next))
       (when (ccl::conditional-store (node-cdr tail) nil new)
         (setf (spin-queue-tail queue) new)
         (return-from push-spin-queue nil)))))

(defun/type pop-spin-queue (queue) (spin-queue) (values t boolean)
  (declare #.*normal-optimize*)
  (let ((node (spin-queue-head queue))
        target)
    (loop
       (loop
          :do (setf node (node-cdr node))
          :unless node :do (return-from pop-spin-queue (values nil nil))
          :do (setf target (node-car node))
          :while (eq target +dummy+))
       (when (ccl::conditional-store (node-car node) target +dummy+)
         (setf (spin-queue-head queue) node)
         (return-from pop-spin-queue (values target t))))))

(defun spin-queue-count (queue)
  (loop
     :with count := 0
     :for node := (spin-queue-head queue) :then (node-cdr node)
     :while node
     :unless (eq (node-car node) +dummy+) :do (incf count)
     :finally (return count)))

(defun/inline spin-queue-empty-p (queue)
  (null (node-cdr (spin-queue-head queue))))

(defun peek-spin-queue (queue)
  (let ((node (spin-queue-head queue))
        target)
    (loop
       :do (setf node (node-cdr node))
       :unless node :do (return-from peek-spin-queue (values nil nil))
       :do (setf target (node-car node))
       :while (eq target +dummy+))
    (values target t)))
