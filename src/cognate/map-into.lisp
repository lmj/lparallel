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

;;; placeholder fix until the next SBCL release

(defun map-into (result-sequence function &rest sequences)
  (declare (dynamic-extent sequences))
  (let ((really-fun (ensure-function function)))
    (macrolet ((map-lambda (params &body body)
                 `(flet ((f ,params ,@body))
                    (declare (dynamic-extent #'f))
                    (if sequences
                        (apply #'map nil #'f sequences)
                        (loop (f))))))
      (etypecase result-sequence
        (vector
         (let ((index 0))
           (declare (type index index))
           (if (and sequences
                    (not (rest sequences))
                    (vectorp (first sequences)))
               ;; 1 vector special case
               (let* ((src (first sequences))
                      (end (min (array-total-size result-sequence)
                                (length src))))
                 (declare (type index end))
                 (declare #.*full-optimize*)
                 (macrolet
                     ((dispatch (ref)
                        `(loop until (eql index end)
                               do (setf (,ref result-sequence index)
                                        (funcall really-fun (aref src index)))
                                  (incf index))))
                   (typecase result-sequence
                     (simple-vector (dispatch svref))
                     (vector        (dispatch aref)))))
               (let ((end (array-total-size result-sequence)))
                 (declare (type index end))
                 (block mapping
                   (macrolet
                       ((dispatch (ref)
                          `(map-lambda (&rest args)
                             (declare (dynamic-extent args))
                             (declare #.*full-optimize*)
                             (when (eql index end)
                               (return-from mapping))
                             (setf (,ref result-sequence index)
                                   (apply really-fun args))
                             (incf index))))
                     (typecase result-sequence
                       (simple-vector (dispatch svref))
                       (vector        (dispatch aref)))))))
           (when (array-has-fill-pointer-p result-sequence)
             (setf (fill-pointer result-sequence) index))))
        (list
         (let ((node result-sequence))
           (map-lambda (&rest args)
             (declare (dynamic-extent args))
             (declare #.*full-optimize*)
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
