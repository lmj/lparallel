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

(defun pmap-into/parts (map-into result-seq fn seqs size parts-hint)
  (let ((input-parts (make-input-parts seqs size parts-hint)))
    (multiple-value-bind (result-parts stitch)
        (make-result-parts result-seq size parts-hint)
      (unwind-protect
           (with-submit-counted
             (map nil
                  (lambda (result-part subseqs)
                    (submit-counted 'apply map-into result-part fn subseqs))
                  result-parts
                  input-parts)
             (receive-counted))
        (when stitch (funcall stitch))))))

(defun map-nil (&rest args)
  (declare (dynamic-extent args))
  (apply #'map nil args))

(defun maplist-into (result-list fn &rest lists)
  "A variation of map-into."
  (let ((fn (ensure-function fn)))
    (apply #'mapl
           (lambda (result &rest args)
             ;; This is an inner loop.
             (declare #.*normal-optimize*)
             (declare (dynamic-extent args))
             (setf (car result) (apply fn args)))
           result-list
           lists)
    result-list))

(defun map-iterate (map size fn seqs)
  "A variation of (map nil ...)/mapc/mapl with size constrained.
Without a result to delineate sublist boundaries, we must enforce them
manually."
  (check-type size (integer 0))
  (let ((fn (ensure-function fn))
        (index 0))
    (apply map
           (lambda (&rest args)
             ;; This is an inner loop.
             (declare #.*normal-optimize*)
             (declare (dynamic-extent args))
             (when (eql index size)
               (return-from map-iterate nil))
             (apply fn args)
             (incf index))
           seqs)))

(defun pmap-into/powder/array (result-seq fn seqs size)
  "When a sequence of size N is divided into N parts, it becomes powder."
  (with-submit-indexed size result-seq
    (let ((index 0))
      (map-iterate #'map-nil
                   size
                   (lambda (&rest args)
                     (declare (dynamic-extent args))
                     (apply #'submit-indexed index fn args)
                     (incf index))
                   seqs))
    (receive-indexed)))

(defun pmap-into/powder/list (map result-seq fn seqs size)
  (let ((result result-seq))
    (with-submit-counted
      (map-iterate map
                   size
                   (lambda (&rest args)
                     (submit-counted (let ((result result))
                                       (lambda ()
                                         (setf (car result) (apply fn args)))))
                     (setf result (cdr result)))
                   seqs)
      (receive-counted))))

(defun pmap-into/powder (map-into result-seq fn seqs size)
  (etypecase result-seq
    (array (pmap-into/powder/array result-seq fn seqs size))
    (list  (let ((map (if (eq map-into #'maplist-into) #'mapl #'map-nil)))
             (pmap-into/powder/list map result-seq fn seqs size)))))

(defun pmap-into/parsed (map-into result-seq fn seqs size parts-hint)
  (when (plusp size)
    (if (eql size (find-num-parts size parts-hint))
        (pmap-into/powder map-into result-seq fn seqs size)
        (pmap-into/parts  map-into result-seq fn seqs size parts-hint)))
  result-seq)

(defun pmap-into/unparsed (map-into result-seq fn seqs)
  (multiple-value-bind (seqs size parts-hint) (%parse-options seqs)
    (let* ((fn (ensure-function fn))
           (initial-fill-pointer (and (arrayp result-seq)
                                      (array-has-fill-pointer-p result-seq)
                                      (fill-pointer result-seq)))
           (parts-hint (get-parts-hint parts-hint))
           (size (or size
                     (let ((limit (if initial-fill-pointer
                                      (array-total-size result-seq)
                                      (length result-seq))))
                       (if seqs
                           (min limit (find-min-length seqs))
                           limit)))))
      (flet ((main ()
               (if seqs
                   (pmap-into/parsed map-into
                                     result-seq fn seqs size parts-hint)
                   (pmap-into/parsed map-into
                                     result-seq
                                     (lambda (x)
                                       (declare #.*normal-optimize*)
                                       (declare (ignore x))
                                       (funcall fn))
                                     (list result-seq)
                                     size
                                     parts-hint))))
        (declare (dynamic-extent #'main))
        (if initial-fill-pointer
            (unwind-protect/ext
             :prepare (setf (fill-pointer result-seq) size)
             :main (main)
             :abort (setf (fill-pointer result-seq) initial-fill-pointer))
            (main))))))

(defun pmap-into (result-sequence function &rest sequences)
  "Parallel version of `map-into'. Keyword arguments `parts' and
`size' are also accepted (see `pmap')."
  (typecase result-sequence
    ((or array list)
     (pmap-into/unparsed #'map-into result-sequence function sequences))
    (t
     (apply #'map-into result-sequence function sequences)))
  result-sequence)

(defun pmap-iterate/parts (map fn seqs size parts-hint)
  (let ((input-parts (make-input-parts seqs size parts-hint)))
    (with-submit-counted
      (with-parts size parts-hint
        (dosequence (subseqs input-parts)
          (next-part)
          (submit-counted 'map-iterate map (part-size) fn subseqs)))
      (receive-counted))))

(defun pmap-iterate/powder (map fn seqs size)
  (with-submit-counted
    (map-iterate map
                 size
                 (lambda (&rest args)
                   (declare (dynamic-extent args))
                   (apply #'submit-counted fn args))
                 seqs)
    (receive-counted)))

(defun pmap-iterate (map fn seqs size parts-hint)
  (if (eql size (find-num-parts size parts-hint))
      (pmap-iterate/powder map fn seqs size)
      (pmap-iterate/parts  map fn seqs size parts-hint))
  nil)

(defun pmap/parsed (result-type function sequences size parts-hint)
  (if result-type
      (pmap-into/parsed #'map-into
                        (make-sequence result-type size)
                        function
                        sequences
                        size
                        parts-hint)
      ;; (pmap nil ...)
      (pmap-iterate #'map-nil function sequences size parts-hint)))

(defun pmap/unparsed (result-type function sequences)
  (with-parsed-options (sequences size parts-hint)
    (pmap/parsed result-type function sequences size parts-hint)))

(defun pmap (result-type function &rest sequences)
  "Parallel version of `map'. Keyword arguments `parts' and `size' are
also accepted.

The `parts' option divides each sequence into `parts' number of parts.
Default is (kernel-worker-count).

The `size' option limits the number of elements mapped to `size'. When
given, no `length' calls are made on the sequence(s) passed.

Warning: `size' must be less than or equal to the length of the
smallest sequence passed. It is unspecified what happens when that
condition is not met."
  (pmap/unparsed result-type function sequences))

(defun pmapcar (function &rest sequences)
  "Parallel version of `mapcar'. Keyword arguments `parts' and `size'
are also accepted (see `pmap').

Unlike `mapcar', `pmapcar' also accepts vectors."
  (pmap/unparsed 'list function sequences))

(defun pmaplist-into (result-list function &rest lists)
  "Like `pmaplist' but results are stored in `result-list'. Keyword
arguments `parts' and `size' are also accepted (see `pmap')."
  (pmap-into/unparsed #'maplist-into result-list function lists))

(defun pmaplist (function &rest lists)
  "Parallel version of `maplist'. Keyword arguments `parts' and `size'
are also accepted (see `pmap')."
  (with-parsed-options (lists size parts-hint)
    (pmap-into/parsed
     #'maplist-into (make-list size) function lists size parts-hint)))

(defun pmapl (function &rest lists)
  "Parallel version of `mapl'. Keyword arguments `parts' and `size'
are also accepted (see `pmap')."
  (with-parsed-options (lists size parts-hint)
    (pmap-iterate #'mapl function lists size parts-hint)
    (first lists)))

(defun pmapc (function &rest lists)
  "Parallel version of `mapc'. Keyword arguments `parts' and `size'
are also accepted (see `pmap')."
  (with-parsed-options (lists size parts-hint)
    (pmap-iterate #'mapc function lists size parts-hint)
    (first lists)))

(defun pmapcan (function &rest lists)
  "Parallel version of `mapcan'. Keyword arguments `parts' and `size'
are also accepted (see `pmap')."
  (declare (dynamic-extent lists))
  (apply #'nconc (apply #'pmapcar function lists)))

(defun pmapcon (function &rest lists)
  "Parallel version of `mapcon'. Keyword arguments `parts' and `size'
are also accepted (see `pmap')."
  (declare (dynamic-extent lists))
  (apply #'nconc (apply #'pmaplist function lists)))

(defun pmap-reduce (map-function reduce-function sequence
                    &rest args
                    &key start end initial-value parts recurse)
  "Equivalent to (preduce reduce-function sequence :key map-function ...)."
  (declare (ignore start end initial-value parts recurse))
  (declare (dynamic-extent args))
  (apply #'preduce reduce-function sequence :key map-function args))
