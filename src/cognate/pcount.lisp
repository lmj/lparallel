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

(defun pcount-if (predicate sequence &key from-end (start 0) end key parts)
  "Parallel version of `count-if'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (let ((subsize (subsize sequence (length sequence) start end)))
    (if (zerop subsize)
        0
        (let ((predicate (ensure-function predicate)))
          (flet ((maybe-inc (acc x)
                   (declare #.*normal-optimize*
                            (fixnum acc))
                   (if (funcall predicate x)
                       (the fixnum (1+ acc))
                       acc)))
            (declare (ftype (function (fixnum t) fixnum) maybe-inc))
            (reduce #'+ (preduce/common #'maybe-inc
                                        sequence
                                        subsize
                                        :initial-value 0
                                        :from-end from-end
                                        :start start
                                        :key key
                                        :parts parts
                                        :partial t)))))))

(defun pcount-if-not (predicate sequence
                      &rest args
                      &key from-end start end key parts)
  "Parallel version of `count-if-not'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args)
           (ignore from-end start end key parts))
  (apply #'pcount-if (complement (ensure-function predicate)) sequence args))

(defun pcount (item sequence
               &key from-end (start 0) end key test test-not parts)
  "Parallel version of `count'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (pcount-if (item-predicate item test test-not) sequence
             :from-end from-end :start start :end end :key key :parts parts))
