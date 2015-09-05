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

;;; Singly-linked queue with compare-and-swap operations.
;;;
;;; The following invariants hold except during updates:
;;;
;;;   (node-car (spin-queue-head queue)) == +dummy+
;;;
;;;   (node-cdr (spin-queue-tail queue)) == nil
;;;
;;;   If the queue is empty, (spin-queue-head queue) == (queue-tail queue).
;;;
;;;   If the queue is non-empty,
;;;   (node-car (node-cdr (spin-queue-head queue))) is the next value
;;;   to be dequeued and (node-car (spin-queue-tail queue)) is the
;;;   most recently enqueued value.
;;;
;;; The CDR of a discarded node is set to +DEAD-END+. This flag must
;;; be checked at each traversal.

(in-package #:lparallel.spin-queue)

;;;; node

#+(or sbcl lispworks)
(progn
  (deftype node () 'cons)
  (alias-function make-node cons)
  (defmacro node-car (node) `(car ,node))
  (defmacro node-cdr (node) `(cdr ,node)))

;;; CCL cannot compare-and-swap on a cons. Slots for defstruct must be
;;; untyped for ccl::conditional-store.
#+ccl
(progn
  (declaim (inline make-node))
  (defstruct (node (:constructor make-node (car cdr)))
    (car (error "no car"))
    (cdr (error "no cdr"))))

;;;; spin-queue

(defconstant +dummy+ 'dummy)
(defconstant +dead-end+ 'dead-end)

(defstruct (spin-queue (:constructor %make-spin-queue (head tail)))
  (head (error "no head") #-ccl :type #-ccl node)
  (tail (error "no tail") #-ccl :type #-ccl node))

(defun make-spin-queue ()
  (let ((dummy (make-node +dummy+ nil)))
    (%make-spin-queue dummy dummy)))

(defun/type push-spin-queue (value queue) (t spin-queue) (values)
  ;; Attempt CAS, repeat upon failure. Upon success update QUEUE-TAIL.
  (declare #.*full-optimize*)
  (let ((new (make-node value nil)))
    (loop (when (cas (node-cdr (spin-queue-tail queue)) nil new)
            (setf (spin-queue-tail queue) new)
            (return (values))))))

(defun/type pop-spin-queue (queue) (spin-queue) (values t boolean)
  ;; Attempt to CAS QUEUE-HEAD with the next node, repeat upon
  ;; failure. Upon success, clear the discarded node and set the CAR
  ;; of QUEUE-HEAD to +DUMMY+.
  (declare #.*full-optimize*)
  (loop (let* ((head (spin-queue-head queue))
               (next (node-cdr head)))
          ;; NEXT could be +DEAD-END+, whereupon we try again.
          (typecase next
            (null (return (values nil nil)))
            (node (when (cas (spin-queue-head queue) head next)
                    (let ((value (node-car next)))
                      (setf (node-cdr head) +dead-end+
                            (node-car next) +dummy+)
                      (return (values value t)))))))))

(defun spin-queue-empty-p (queue)
  (null (node-cdr (spin-queue-head queue))))

(defun try-each-elem (fun queue)
  (let ((node (spin-queue-head queue)))
    (loop
       (let ((value (node-car node)))
         (unless (eq value +dummy+)
           (funcall fun value)))
       (setf node (node-cdr node))
       (cond ((eq node +dead-end+)
              (return nil))
             ((null node)
              (return t))))))

(defun spin-queue-count (queue)
  (tagbody
   :retry
     (let ((count 0))
       (unless (try-each-elem (lambda (elem)
                                (declare (ignore elem))
                                (incf count))
                              queue)
         (go :retry))
       (return-from spin-queue-count count))))

(defun peek-spin-queue (queue)
  (loop until (try-each-elem (lambda (elem)
                               (return-from peek-spin-queue (values elem t)))
                             queue))
  (values nil nil))
