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

(defun has-lambda-keywords-p (list)
  (some (lambda (p) (find p lambda-list-keywords)) list))

#-lparallel.with-debug
(progn
  (defmacro defun/inline (name params &body body)
    "Shortcut for 
       (declaim (inline foo)) 
       (defun foo ...)."
    `(progn
       (declaim (inline ,name))
       (defun ,name ,params ,@body)))

  (defmacro defun/type (name params arg-types return-type &body body)
    "Shortcut for 
       (declaim (ftype (function arg-types return-type) foo) 
       (defun foo ...)."
    (with-parsed-body (docstring declares body)
      `(progn
         (declaim (ftype (function ,arg-types ,return-type) ,name))
         (defun ,name ,params
           ,@(unsplice docstring)
           ,@declares
           ;; for a simple arg list, also declare types
           ,@(when (not (has-lambda-keywords-p params))
               (loop
                  :for type  :in arg-types
                  :for param :in params
                  :collect `(declare (type ,type ,param))))
           (the ,return-type (progn ,@body))))))

  (defmacro defun/type/inline (name params arg-types return-type &body body)
    `(progn
       (declaim (inline ,name))
       (defun/type ,name ,params ,arg-types ,return-type ,@body))))

;;; Since return types are not always checked, check manually.
#+lparallel.with-debug
(progn
  (defmacro defun/type (name params arg-types return-type &body body)
    (with-parsed-body (docstring declares body)
      `(progn
         (declaim (ftype (function ,arg-types ,return-type) ,name))
         (defun ,name ,params
           ,@(unsplice docstring)
           ,@declares
           ;; for a simple arg list, check types
           ,@(when (not (has-lambda-keywords-p params))
               (loop
                  :for type  :in arg-types
                  :for param :in params
                  :collect `(check-type ,param ,type)))
           ;; for a simple values list, check types
           ,(if (has-lambda-keywords-p (mklist return-type))
                `(progn ,@body)
                (let* ((return-types (if (and (consp return-type)
                                              (eq 'values (car return-type)))
                                         (cdr return-type)
                                         (list return-type)))
                       (return-vars (mapcar (lambda (x)
                                              (if (symbolp x)
                                                  (gensym (symbol-name x))
                                                  (gensym)))
                                            return-types)))
                  `(multiple-value-bind ,return-vars (progn ,@body)
                     ,@(loop
                          :for type :in return-types
                          :for var  :in return-vars
                          :collect `(check-type ,var ,type))
                     (values ,@return-vars))))))))

  (setf (macro-function 'defun/inline) (macro-function 'defun))
  (setf (macro-function 'defun/type/inline) (macro-function 'defun/type)))
