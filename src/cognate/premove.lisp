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

(defun premove-if-not/list (test list from-end start end key parts)
  (let* ((size (length list))
         (subsize (subsize list size start end)))
    (if (zerop subsize)
        nil
        (let ((test (ensure-function test))
              (leading (subseq list 0 start))
              (trailing (if (or (null end)
                                (eql subsize (- size start)))
                            nil
                            (copy-list (nthcdr end list)))))
          (nconc leading
                 (reduce #'nreconc
                         (preduce/common (lambda (acc x)
                                           (declare #.*normal-optimize*)
                                           (if (funcall test x)
                                               (cons x acc)
                                               acc))
                                         (nthcdr start list)
                                         subsize
                                         :initial-value nil
                                         :key key
                                         :parts parts
                                         :from-end from-end
                                         :partial t)
                         :initial-value trailing
                         :from-end t))))))

(defun premove-if-not (test sequence
                       &rest args
                       &key from-end (start 0) end key parts)
  "Parallel version of `remove-if-not'. Note the `count' option is not
supported.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args))
  (typecase sequence
    (list      (premove-if-not/list test sequence
                                    from-end start end key parts))
    (otherwise (apply #'remove-if-not test sequence args))))

(defun premove-if (test sequence
                   &rest args
                   &key from-end (start 0) end key parts)
  "Parallel version of `remove-if'. Note the `count' option is not
supported.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args))
  (typecase sequence
    (list (premove-if-not/list (complement (ensure-function test)) sequence
                               from-end start end key parts))
    (otherwise (apply #'remove-if test sequence
                      (remove-from-plist args :parts)))))

(defun premove (item sequence
                &rest args
                &key test test-not from-end (start 0) end key parts)
  "Parallel version of `remove'. Note the `count' option is not
supported.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args))
  (typecase sequence
    (list (premove-if-not/list (complement (item-predicate item test test-not))
                               sequence
                               from-end start end key parts))
    (otherwise (apply #'remove item sequence
                      (remove-from-plist args :parts)))))
