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

(defun pquantifier (quantifier predicate sequences bail)
  (with-parsed-options (sequences size parts-hint)
    (let ((input-parts (make-input-parts sequences size parts-hint)))
      (with-submit-cancelable
        (dosequence (subseqs input-parts)
          (submit-cancelable 'apply quantifier predicate subseqs))
        (receive-cancelables result
          (when (eq bail (to-boolean result))
            (return-from pquantifier result))))))
  (not bail))

(defun pevery (predicate &rest sequences)
  "Parallel version of `every'. Calls to `predicate' are done in
parallel, though not necessarily at the same time. Behavior is
otherwise indistinguishable from `every'.

Keyword arguments `parts' and `size' are also accepted (see `pmap')."
  (pquantifier #'every (ensure-function predicate) sequences nil))

(defun psome (predicate &rest sequences)
  "Parallel version of `some'. Calls to `predicate' are done in
parallel, though not necessarily at the same time. Behavior is
otherwise indistinguishable from `some' except that any non-nil
predicate comparison result may be returned.

Keyword arguments `parts' and `size' are also accepted (see `pmap')."
  (pquantifier #'some (ensure-function predicate) sequences t))

(defun pnotevery (predicate &rest sequences)
  "Parallel version of `notevery'. Calls to `predicate' are done in
parallel, though not necessarily at the same time. Behavior is
otherwise indistinguishable from `notevery'.

Keyword arguments `parts' and `size' are also accepted (see `pmap')."
  (declare (dynamic-extent sequences))
  (not (apply #'pevery predicate sequences)))

(defun pnotany (predicate &rest sequences)
  "Parallel version of `notany'. Calls to `predicate' are done in
parallel, though not necessarily at the same time. Behavior is
otherwise indistinguishable from `notany'.

Keyword arguments `parts' and `size' are also accepted (see `pmap')."
  (declare (dynamic-extent sequences))
  (not (apply #'psome predicate sequences)))
