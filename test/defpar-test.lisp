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

(in-package #:lparallel-test)

(define-plet-test defpar-basic-test defpar-basic-test-fn defpar)

(defpar defpar-accept ()
  (let1 queue (make-queue)
    (plet ((outer (progn
                    (sleep 0.6)
                    (push-queue :outer queue))))
      ;; placate warnings
      (setf *memo* (lambda () outer))
      (sleep 0.1)
      (plet ((inner1 (sleep 0.4))
             (inner2 (sleep 0.4)))
        (setf *memo* inner1)
        (setf *memo* inner2)
        (push-queue :inner queue)))
        ;; inner plet was parallelized
        (is (eq :inner (pop-queue queue)))))

(lp-base-test defpar-accept-test
  (with-new-kernel (3)
    (sleep 0.1)
    (defpar-accept)))

(defpar defpar-reject ()
  (let1 queue (make-queue)
    (plet ((outer1 (progn
                     (sleep 0.4)
                     (push-queue :outer queue)))
           (outer2 (progn
                     (sleep 0.4)
                     (push-queue :outer queue))))
      ;; placate warnings
      (setf *memo* (lambda () outer1))
      (setf *memo* (lambda () outer2))
      (sleep 0.1)
      (plet ((inner1 (sleep 0.2))
             (inner2 (sleep 0.2)))
        (setf *memo* inner1)
        (setf *memo* inner2)
        (push-queue :inner queue)))
    ;; inner plet was not parallelized
    (is (eq :outer (pop-queue queue)))))
  
(lp-base-test defpar-reject-test
  (with-new-kernel (2)
    (sleep 0.1)
    (defpar-reject)))

(defun fib-let (n)
  (if (< n 2)
      n
      (let ((a (fib-let (- n 1)))
            (b (fib-let (- n 2))))
        (+ a b))))

(defpar fib-plet (n)
  (if (< n 2)
      n
      (plet ((a (fib-plet (- n 1)))
             (b (fib-plet (- n 2))))
        (+ a b))))

(defpar fib-plet-if (n)
  (if (< n 2)
      n
      (plet-if (> n 5) ((a (fib-plet-if (- n 1)))
                        (b (fib-plet-if (- n 2))))
        (+ a b))))

(lp-test defpar-fib-test
  (loop
     :for n :from 1 :to 15
     :do (is (= (fib-let n) (fib-plet n) (fib-plet-if n)))))
