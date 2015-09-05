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

(defun find-num-parts (size parts-hint)
  (multiple-value-bind (quo rem) (floor size parts-hint)
    (values (if (zerop quo) rem parts-hint) quo rem)))

(defmacro with-parts (seq-size parts-hint &body body)
  (with-gensyms (quo rem index num-parts part-offset part-size)
    `(multiple-value-bind
           (,num-parts ,quo ,rem) (find-num-parts ,seq-size ,parts-hint)
       (declare (fixnum ,num-parts ,quo ,rem))
       (let ((,index       0)
             (,part-offset 0)
             (,part-size   0))
         (declare (fixnum ,index ,part-offset ,part-size))
         (flet ((next-part ()
                  (when (< ,index ,num-parts)
                    (unless (zerop ,index)
                      (incf ,part-offset ,part-size))
                    (setf ,part-size (if (< ,index ,rem) (1+ ,quo) ,quo))
                    (incf ,index)))
                (part-size   () ,part-size)
                (part-offset () ,part-offset)
                (num-parts   () ,num-parts))
           (declare (inline part-size part-offset num-parts)
                    (ignorable #'part-size #'part-offset #'num-parts))
           ,@body)))))

(defun subdivide-array (array size parts-hint)
  (with-parts size parts-hint
    (map-into (make-array (num-parts))
              (lambda ()
                (next-part)
                (make-array (part-size)
                            :displaced-to array
                            :displaced-index-offset (part-offset)
                            :element-type (array-element-type array))))))

(defun subdivide-list (list size parts-hint)
  (with-parts size parts-hint
    (loop with p = list
          while (next-part)
          collect p
          do (setf p (nthcdr (part-size) p)))))

(defun subdivide-list/slice (list size parts-hint)
  (with-parts size parts-hint
    (loop with p = list
          while (next-part)
          collect p into firsts
          collect (prog1 (setf p (nthcdr (1- (part-size)) p))
                    (setf p (prog1 (cdr p) (setf (cdr p) nil)))) into lasts
          finally (return (values firsts
                                  (lambda ()
                                    ;; stitch it back together
                                    (loop for last  in lasts
                                          for first in (cdr firsts)
                                          do (setf (cdr last) first)
                                          finally (setf (cdr last) p))))))))

(defun make-parts (result size parts-hint &key slicep)
  (if (listp result)
      (funcall (if slicep #'subdivide-list/slice #'subdivide-list)
               result size parts-hint)
      (subdivide-array result size parts-hint)))

(defun make-result-parts (result size parts-hint)
  "Subdivide the result sequence. For a list, delineate boundaries by slicing."
  (make-parts result size parts-hint :slicep t))

(defun make-input-parts (sequences size parts-hint)
  "Subdivide and interleave sequences for parallel mapping."
  (zip/vector (mapcar (lambda (seq) (make-parts seq size parts-hint))
                      sequences)))
