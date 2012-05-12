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

(in-package #:lparallel.cognate)

(defun zip/vector (seqs)
  (apply #'map 'vector (lambda (&rest args) args) seqs))

(defun find-min-length (seqs)
  (reduce #'min seqs :key #'length))

(defmacro/once build-vector (&once n &body body)
  "Execute `body' `n' times, collecting the results into a vector."
  (with-gensyms (result index)
    `(let1 ,result (make-array ,n)
       (dotimes (,index ,n ,result)
         (setf (aref ,result ,index) (progn ,@body))))))

(defun item-predicate (item test test-not)
  (when (and test test-not)
    (error "Both :TEST and :TEST-NOT options given."))
  (when test-not
    (setf test (complement test-not))
    (setf test-not nil))
  (if test
      (lambda (x) (funcall test item x))
      (typecase item
        ((or number character)
         (lambda (x)
           ;; in inner loop
           (declare #.*normal-optimize*)
           (eql item x)))
        (otherwise
         (lambda (x)
           ;; in inner loop
           (declare #.*normal-optimize*)
           (eq item x))))))

(defun subsize (seq size start end)
  (let1 result (- (or end size) start)
    (when (or (minusp result) (> result size))
      (error "Bad interval for sequence operation on ~a: start=~a end=~a"
             seq start end))
    result))

(defun remove-prop (target-key plist)
  (loop
     :for (key value) :on plist :by #'cddr
     :unless (eq key target-key)
     :nconc (list key value)))

(defun remove-props (keys plist)
  (loop
     :for (key value) :on plist :by #'cddr
     :unless (member key keys)
     :nconc (list key value)))
