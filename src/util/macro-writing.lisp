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

(in-package #:lparallel.util)

(defmacro with-gensyms (names &body body)
  `(let ,(loop
            :for name :in names
            :collect `(,name (gensym ,(symbol-name name))))
     ,@body))

(defmacro once-only-1 (var &body body)
  (let ((tmp (gensym (symbol-name var))))
    ``(let ((,',tmp ,,var))
        ,(let ((,var ',tmp))
           ,@body))))

(defmacro once-only (vars &body body)
  (if vars
      `(once-only-1 ,(car vars)
         (once-only ,(cdr vars)
           ,@body))
      `(progn ,@body)))

(defun mklist (x)
  (if (listp x) x (list x)))

(defun unsplice (form)
  (if form (list form) nil))

(defun conc-syms (&rest syms)
  (apply #'concatenate 'string (mapcar #'symbol-name syms)))

(defun intern-conc (&rest syms)
  "Concatenate `syms' then intern the result."
  (intern (apply #'conc-syms syms)))

(defun make-symbol-conc (&rest syms)
  "Concatenate `syms' then make-symbol the result."
  (make-symbol (apply #'conc-syms syms)))

(defun has-docstring-p (body)
  (and (stringp (car body)) (cdr body)))

(defun has-declare-p (body)
  (and (consp (car body)) (eq (caar body) 'declare)))

(defun parse-body (body &key documentation whole)
  (loop
     :for docstring-next-p := (and documentation (has-docstring-p body))
     :for declare-next-p   := (has-declare-p body)
     :while (or docstring-next-p declare-next-p)
     :when docstring-next-p :collect (pop body) :into docstrings
     :when declare-next-p   :collect (pop body) :into declares
     :finally (progn
                (unless (<= (length docstrings) 1)
                  (error "Too many documentation strings in ~S."
                         (or whole body)))
                (return (values body declares (first docstrings))))))

(defmacro with-parsed-body ((docstring declares body) &body own-body)
  "Pop declarations and maybe docstring off `body-var' and assign them
to `preamble'."
  (if docstring
      `(multiple-value-bind
             (,body ,declares ,docstring) (parse-body ,body :documentation t)
         ,@own-body)
      `(multiple-value-bind
             (,body ,declares) (parse-body ,body)
         ,@own-body)))
