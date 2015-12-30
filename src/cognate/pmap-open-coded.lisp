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

;;;; util

(defmacro check-symbols (&rest syms)
  `(progn
     ,@(loop for sym in syms
             collect `(check-type ,sym symbol))))

(defmacro defmacro/syms (name params &body body)
  "Like `defmacro' but requires all parameters to be symbols."
  (with-parsed-body (body declares docstring)
    `(defmacro ,name ,params
       ,@(unsplice docstring)
       ,@declares
       (check-symbols ,@params)
       ,@body)))

(defun quotedp (form)
  (and (consp form)
       (eq (first form) 'quote)))

(defun quoted-vector-type-p (form)
  (and (quotedp form)
       ;; I pity nil vector types. Don't you?
       (not (null (second form)))
       (subtypep (second form) 'vector)))

;;;; vector-into-vector mapping

(defmacro/syms map-into/vector/1-vector/range (dst fn src start end)
  (with-gensyms (index)
    `(let ((,index ,start))
       (declare (type index ,index))
       (loop until (eql ,index ,end)
             do (setf (aref ,dst ,index) (funcall ,fn (aref ,src ,index)))
                (incf ,index)))))

(defmacro/syms pmap-into/vector/1-vector (dst fn src size parts)
  (with-gensyms (start end)
    `(let ((,start 0))
       (declare (type index ,start))
       (with-parts ,size ,parts
         (with-submit-counted
           (loop while (next-part)
                 do (submit-counted (let ((,start ,start)
                                          (,end (+ ,start (part-size))))
                                      (declare (type index ,start ,end))
                                      (lambda ()
                                        (map-into/vector/1-vector/range
                                         ,dst ,fn ,src ,start ,end))))
                    (incf ,start (part-size)))
           (receive-counted)))
       (when (array-has-fill-pointer-p ,dst)
         (setf (fill-pointer ,dst) ,size))
       ,dst)))

;;;; PMAP-INTO and PMAP

(define-compiler-macro pmap-into (&whole whole
                                  result-sequence function &rest args)
  "Open-coding for 1 vector mapped to vector."
  (multiple-value-bind (sequences size-form parts-form) (%parse-options args)
    (if (eql 1 (length sequences))
        (with-gensyms (dst fn src size parts)
          `(let* ((,src ,(first sequences))
                  (,dst ,result-sequence)
                  (,size (or ,size-form
                             (min (if (and (vectorp ,dst)
                                           (array-has-fill-pointer-p ,dst))
                                      (array-total-size ,dst)
                                      (length ,dst))
                                  (length ,src))))
                  (,fn (ensure-function ,function))
                  (,parts (get-parts-hint ,parts-form)))
             (if (and (vectorp ,dst)
                      (vectorp ,src)
                      (plusp ,size))
                 (pmap-into/vector/1-vector ,dst ,fn ,src ,size ,parts)
                 (locally (declare (notinline pmap-into))
                   (pmap-into ,dst ,fn :size ,size :parts ,parts ,src)))))
        whole)))

(define-compiler-macro pmap (&whole whole result-type function &rest args)
  "Open-coding for 1 vector mapped to vector."
  (multiple-value-bind (sequences size-form parts-form) (%parse-options args)
    (if (and (eql 1 (length sequences))
             ;; reject literal result-type of nil, 'list, etc immediately
             (not (null result-type))
             (or (not (quotedp result-type))
                 (quoted-vector-type-p result-type)))
        (with-gensyms (dst fn src size parts result-type-value)
          `(let* ((,src ,(first sequences))
                  (,size (or ,size-form (length ,src)))
                  (,fn (ensure-function ,function))
                  (,parts (get-parts-hint ,parts-form))
                  (,result-type-value ,result-type))
             (if ,result-type-value
                 (let ((,dst (make-sequence
                              ;; attempt to use the literal result-type
                              ,(if (quoted-vector-type-p result-type)
                                   result-type
                                   result-type-value)
                              ,size)))
                   (if (and (vectorp ,dst)
                            (vectorp ,src)
                            (plusp ,size))
                       (pmap-into/vector/1-vector ,dst ,fn ,src ,size ,parts)
                       (locally (declare (notinline pmap-into))
                         (pmap-into ,dst ,fn :size ,size :parts ,parts ,src))))
                 (locally (declare (notinline pmap))
                   (pmap nil ,fn :size ,size :parts ,parts ,src)))))
        whole)))
