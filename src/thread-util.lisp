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

(in-package #:lparallel.thread-util)

(defmacro with-thread ((&key bindings name) &body body)
  `(let1 *default-special-bindings* ,bindings
     (make-thread (lambda () ,@body) :name ,name)))

(defmacro/once with-lock-predicate/no-wait (&once lock predicate &body body)
  ;; predicate intentionally evaluated twice
  `(when (and ,predicate (acquire-lock ,lock nil))
     (unwind-protect
          (when ,predicate
            ,@body)
       (release-lock ,lock))))

(defmacro/once with-lock-predicate/wait (&once lock predicate &body body)
  ;; predicate intentionally evaluated twice
  `(when ,predicate
     (with-lock-held (,lock)
       (when ,predicate
         ,@body))))

(defmacro define-thread-locals (result &rest defvar-forms)
  `(progn
     (defvar ,result nil)
     ,@(loop
          :for (name value doc) :in defvar-forms
          :collect `(defvar ,name ,value ,doc)
          :collect `(unless (assoc ',name ,result)
                      (push (cons ',name ',value) ,result)))))

(defmacro define-locking-fn/base (name args ftype
                                  lock-reader
                                  defun/no-lock
                                  ftype/no-lock
                                  &body body)
  (let1 name/no-lock (intern-conc name '#:/no-lock)
    `(progn
       (,defun/no-lock ,name/no-lock ,args ,@(unsplice ftype/no-lock) ,@body)
       (defun/ftype ,name ,args ,ftype
         (declare #.*normal-optimize*)
         (with-lock-held ((,lock-reader ,(car (last args))))
           (,name/no-lock ,@args))))))

(defmacro define-locking-fn (name args ftype lock &body body)
  `(define-locking-fn/base ,name ,args ,ftype ,lock defun/ftype ,ftype
     (declare #.*normal-optimize*)
     ,@body))

(defmacro define-simple-locking-fn (name args ftype lock &body body)
  `(define-locking-fn/base ,name ,args ,ftype ,lock defun/inline nil
     (declare #.*normal-optimize*)
     ,@body))

#+allegro
(defun condition-notify-and-yield (cvar)
  (condition-notify cvar)
  (thread-yield))

#-allegro
(setf (symbol-function 'condition-notify-and-yield) #'condition-notify)
