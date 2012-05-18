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

;;; SBCL has a very slow, much-consing implementation of `map-into'
;;; which affects most pmap functions.

;;; I submitted a `map-into' replacement to SBCL which is pending
;;; review. A userland version of that submission follows.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'map-into))

(deftype index () `(integer 0 #.array-dimension-limit))

;;; Uses the machinery of (MAP NIL ...). For non-vectors we avoid
;;; computing the length of the result sequence since we can detect
;;; the end during mapping (if MAP even gets that far).
(defun map-into (result-sequence function &rest sequences)
  (declare (dynamic-extent sequences))
  (let ((really-fun (%coerce-callable-to-fun function)))
    ;; For each result type, define a mapping function which is
    ;; responsible for replacing RESULT-SEQUENCE elements and for
    ;; terminating itself if the end of RESULT-SEQUENCE is reached.
    ;;
    ;; The mapping function is defined with MAP-LAMBDA, whose syntax
    ;; matches that of LAMBDA. Note (MAP-INTO SEQ (LAMBDA () ...)) is
    ;; a different animal, hence the awkward flip between MAP and LOOP
    ;; in the definition of MAP-LAMBDA.
    (macrolet ((map-lambda (params &body body)
                 `(flet ((f ,params ,@body))
                    (declare (dynamic-extent #'f))
                    (if sequences
                        (apply #'map nil #'f sequences)
                        (loop (f))))))
      ;; Optimize MAP-LAMBDAs since they are the inner loops. Because
      ;; we are manually doing bounds checking with known types, turn
      ;; off safety for vectors and lists but keep it for generic
      ;; sequences.
      (etypecase result-sequence
        (vector
         (let ((index 0)
               (end (array-dimension result-sequence 0)))
           (declare (type index index))
           (block mapping
             ;; might as well optimize vector types
             (macrolet
                 ((map-into-vector (type ref)
                    `(map-lambda (&rest args)
                       (declare (dynamic-extent args)
                                (optimize speed (safety 0)))
                       (when (eql index end)
                         (return-from mapping))
                       (setf (,ref (the ,type result-sequence) index)
                             (apply really-fun args))
                       (incf index))))
               (typecase result-sequence
                 (simple-vector (map-into-vector simple-vector svref))
                 (simple-string (map-into-vector simple-string aref))
                 (vector        (map-into-vector vector aref)))))
           (when (array-has-fill-pointer-p result-sequence)
             (setf (fill-pointer result-sequence) index))))
        (list
         (let ((node result-sequence))
           (map-lambda (&rest args)
             (declare (dynamic-extent args) (optimize speed (safety 0)))
             (when (null node)
               (return-from map-into result-sequence))
             (setf (car node) (apply really-fun args))
             (setf node (cdr node)))))
        (sequence
         (multiple-value-bind (iter limit from-end)
             (sb-sequence:make-sequence-iterator result-sequence)
           (map-lambda (&rest args)
             (declare (dynamic-extent args) (optimize speed))
             (when (sb-sequence:iterator-endp result-sequence
                                              iter limit from-end)
               (return-from map-into result-sequence))
             (setf (sb-sequence:iterator-element result-sequence iter)
                   (apply really-fun args))
             (setf iter (sb-sequence:iterator-step result-sequence
                                                   iter from-end))))))))
  result-sequence)
