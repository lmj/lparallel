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

(in-package #:lparallel.cognate)

(defmacro pop-plist (list)
  (check-type list symbol)
  `(loop 
      :while (keywordp (car ,list))
      :collect (pop ,list)
      :collect (pop ,list)))

(defmacro pop-keyword-args (list &rest keys)
  (check-type list symbol)
  (with-gensyms (plist)
    `(when-let (,plist (pop-plist ,list))
       (destructuring-bind (&key ,@keys) ,plist
         (values ,@keys)))))

(defun get-parts-hint (parts-hint)
  (cond (parts-hint
         (check-type parts-hint (integer 1))
         parts-hint)
        (t
         (kernel-worker-count))))

(defmacro/once with-parsed-options ((seqs size parts-hint
                                     &key &once result-size)
                                    &body body)
  (check-type seqs symbol)
  (check-type size symbol)
  (check-type parts-hint symbol)
  `(multiple-value-bind (,parts-hint ,size) (pop-keyword-args ,seqs parts size)
     (unless ,seqs
       (error "Input sequence(s) for parallelization not found."))
     (unless ,size
       (setf ,size (if ,result-size
                       (min ,result-size (find-min-length ,seqs))
                       (find-min-length ,seqs))))
     (setf ,parts-hint (get-parts-hint ,parts-hint))
     ,@body))
