;;; Copyright (c) 2014, James M. Lawrence. All rights reserved.
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

(defpackage #:lparallel.slet
  (:documentation "(private) Serial let.")
  (:use #:cl
        #:lparallel.util)
  (:export #:slet)
  (:import-from #:alexandria
                #:ensure-list))

(in-package #:lparallel.slet)

(defun parse-bindings (bindings)
  (let ((mv-bindings nil)
        (null-bindings nil))
    (dolist (binding bindings)
      (etypecase binding
        (cons (if (= 1 (length binding))
                  (dolist (var (ensure-list (first binding)))
                    (push var null-bindings))
                  (destructuring-bind (var-or-vars form) binding
                    (push `(,(ensure-list var-or-vars) ,form)
                          mv-bindings))))
        (symbol (push binding null-bindings))))
    (values (reverse mv-bindings)
            (reverse null-bindings))))

;;; To ensure that `slet' is interchangeable with `plet', use
;;; temporaries to avoid `let*'-like behavior.

(defun make-temp-var (var)
  (gensym (symbol-name var)))

(defun make-binding-datum (mv-binding)
  (destructuring-bind (vars form) mv-binding
    `(,vars ,(mapcar #'make-temp-var vars) ,form)))

(defun make-binding-data (bindings)
  (multiple-value-bind (mv-bindings null-bindings) (parse-bindings bindings)
    (values (mapcar #'make-binding-datum mv-bindings)
            null-bindings)))

(defmacro bind ((vars form) &body body)
  (if (= 1 (length vars))
      `(let ((,(first vars) ,form))
         ,@body)
      `(multiple-value-bind ,vars ,form
         ,@body)))

(defmacro %slet (binding-data full-binding-data null-bindings body)
  (if binding-data
      (destructuring-bind
            ((vars temp-vars form) &rest more-binding-data) binding-data
        (declare (ignore vars))
        `(bind (,temp-vars ,form)
           (%slet ,more-binding-data ,full-binding-data ,null-bindings ,body)))
      `(let (,@null-bindings
             ,@(loop for (vars temp-vars nil) in full-binding-data
                     append (mapcar #'list vars temp-vars)))
         ,@body)))

(defmacro slet (bindings &body body)
  "`slet' (serial let) is the non-parallel counterpart to `plet'.

The syntax of `slet' matches that of `plet', which includes the
ability to bind multiple values."
  (multiple-value-bind (binding-data null-bindings) (make-binding-data bindings)
    `(%slet ,binding-data ,binding-data ,null-bindings ,body)))
