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

(defmacro with-preduce-context (size parts &body body)
  (with-gensyms (results)
    `(with-parts ,size ,parts
       (let ((,results (make-array (num-parts))))
         (with-submit-indexed (num-parts) ,results
           ,@body
           (receive-indexed))))))

(defun preduce-partial/vector (function sequence start size parts
                               &rest keyword-args)
  (declare (dynamic-extent keyword-args))
  (with-preduce-context size parts
    (loop for result-index from 0
          while (next-part)
          do (apply #'submit-indexed
                    result-index
                    #'reduce
                    function
                    sequence
                    :start (+ start (part-offset))
                    :end   (+ start (part-offset) (part-size))
                    keyword-args))))

(defun preduce-partial/list (function sequence start size parts
                             &rest keyword-args)
  (declare (dynamic-extent keyword-args))
  (with-preduce-context size parts
    (loop with subseq = (nthcdr start sequence)
          for result-index from 0
          while (next-part)
          do (apply #'submit-indexed
                    result-index
                    #'reduce
                    function
                    subseq
                    :end (part-size)
                    keyword-args)
             (setf subseq (nthcdr (part-size) subseq)))))

(defun %preduce-partial (function sequence start size parts
                         &rest keyword-args)
  (declare (dynamic-extent keyword-args))
  (etypecase sequence
    (vector (apply #'preduce-partial/vector
                   function sequence start size parts keyword-args))
    (list  (apply #'preduce-partial/list
                  function sequence start size parts keyword-args))))

(defun preduce/common (function sequence subsize
                       &key
                       key
                       from-end
                       (start 0)
                       end
                       (initial-value nil initial-value-given-p)
                       parts
                       recurse
                       partial)
  (declare (ignore end))
  (cond ((zerop subsize)
         (when partial
           (error "PREDUCE-PARTIAL given zero-length sequence"))
         (if initial-value-given-p
             initial-value
             (funcall function)))
        (t
         (let* ((parts-hint (get-parts-hint parts))
                (results    (apply #'%preduce-partial
                                   function sequence start subsize parts-hint
                                   :key key
                                   :from-end from-end
                                   (when initial-value-given-p
                                     (list :initial-value initial-value)))))
           (if partial
               results
               (let ((new-size (length results)))
                 (if (and recurse (>= new-size 4))
                     (apply #'preduce/common
                            function
                            results
                            new-size
                            :from-end from-end
                            :parts (min parts-hint (floor new-size 2))
                            :recurse recurse
                            (when initial-value-given-p
                              (list :initial-value initial-value)))
                     (reduce function results))))))))

(defun preduce (function sequence &rest args
                &key key from-end (start 0) end initial-value parts recurse)
  "Parallel version of `reduce'.

`preduce' subdivides the input sequence into `parts' number of parts
and, in parallel, calls `reduce' on each part. The partial results are
then reduced again, either by `reduce' (the default) or, if `recurse'
is non-nil, by `preduce'.

`parts' defaults to (kernel-worker-count).

`key' is thrown out while reducing the partial results. It applies to
the first pass only.

`start' and `end' have the same meaning as in `reduce'.

`from-end' means \"from the end of each part\".

`initial-value' means \"initial value of each part\"."
  (declare (ignore key from-end initial-value parts recurse))
  (declare (dynamic-extent args))
  (typecase sequence
    ((or vector list)
     (apply #'preduce/common
            function
            sequence
            (subsize sequence (length sequence) start end)
            args))
    (otherwise
     (apply #'reduce
            function
            sequence
            (remove-from-plist args :parts :recurse)))))

(defun preduce-partial (function sequence &rest args
                        &key key from-end (start 0) end initial-value parts)
  "Like `preduce' but only does a single reducing pass.

The length of `sequence' must not be zero.

Returns the partial results as a vector."
  (declare (ignore key from-end initial-value parts))
  (declare (dynamic-extent args))
  (apply #'preduce/common
         function
         sequence
         (subsize sequence (length sequence) start end)
         :partial t
         args))
