;;; Copyright (c) 2011, James M. Lawrence. All rights reserved.
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

(in-package #:lparallel-bench)

(defmacro collect-n (n &body body)
  "Execute `body' `n' times, collecting the results into a list."
  `(loop :repeat ,n :collect (progn ,@body)))

(defmacro once-only/rest (forms &body body)
  "Like `once-only' but for a `&rest' parameter."
  (with-gensyms (results)
    `(let ((,forms (collect-n (length ,forms)
                     (gensym (symbol-name ',forms))))
           (,results ,forms))
       `(let ,(mapcar #'list ,forms ,results)
          ,,@body))))

(defmacro/once curry (&once fn &rest args)
  (once-only/rest args
    `(lambda (&rest free-args)
       (apply ,fn ,@args free-args))))

(defun uncurry (fn)
  (lambda (x) (declare (ignore x)) (funcall fn)))

(defun make-random-seq (type size)
  (let1 seq (make-sequence type size)
    (map-into seq (uncurry (curry 'random 1.0)) seq)))

(defun make-random-list (size)
  (make-random-seq 'list size))

(defun make-random-vector (size)
  (let1 seq (make-array size :element-type 'single-float)
    (map-into seq (uncurry (curry 'random 1.0)) seq)))
