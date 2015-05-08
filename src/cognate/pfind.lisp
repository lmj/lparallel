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

(defmacro with-pfind-context (sequence start end parts &body body)
  (with-gensyms (top result)
    `(block ,top
       (with-parts
           (subsize ,sequence (length ,sequence) ,start ,end)
           (get-parts-hint ,parts)
         (with-submit-cancelable
           ,@body
           (receive-cancelables ,result
             (when ,result
               (return-from ,top ,result)))
           nil)))))

(defun pfind-if/vector (predicate sequence
                        &key from-end (start 0) end key parts)
  (with-pfind-context sequence start end parts
    (loop with index = start
          while (next-part)
          do (submit-cancelable #'find-if
                                predicate
                                sequence
                                :from-end from-end
                                :start index
                                :end (+ index (part-size))
                                :key key)
             (incf index (part-size)))))

(defun pfind-if/list (predicate sequence
                      &key from-end (start 0) end key parts)
  (with-pfind-context sequence start end parts
    (loop with sublist = (nthcdr start sequence)
          while (next-part)
          do (submit-cancelable #'find-if
                                predicate
                                sublist
                                :from-end from-end
                                :end (part-size)
                                :key key)
             (setf sublist (nthcdr (part-size) sublist)))))

(defun pfind-if (predicate sequence
                 &rest args
                 &key from-end start end key parts)
  "Parallel version of `pfind-if'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args)
           (ignore from-end start end key parts))
  (let ((predicate (ensure-function predicate)))
    (typecase sequence
      (vector    (apply #'pfind-if/vector predicate sequence args))
      (list      (apply #'pfind-if/list   predicate sequence args))
      (otherwise (apply #'find-if predicate sequence
                        (remove-from-plist args :parts))))))

(defun pfind-if-not (predicate sequence
                     &rest args
                     &key from-end start end key parts)
  "Parallel version of `pfind-if-not'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args)
           (ignore from-end start end key parts))
  (apply #'pfind-if (complement (ensure-function predicate)) sequence args))

(defun pfind (item sequence
              &rest args
              &key from-end test test-not start end key parts)
  "Parallel version of `pfind'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args)
           (ignore from-end start end key parts))
  (apply #'pfind-if
         (item-predicate item test test-not)
         sequence
         (remove-from-plist args :test :test-not)))
