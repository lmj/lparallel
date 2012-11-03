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

(in-package #:lparallel.thread-util)

(import-now bordeaux-threads:*default-special-bindings*
            bordeaux-threads:make-thread
            bordeaux-threads:condition-notify
            bordeaux-threads:acquire-lock
            bordeaux-threads:release-lock)

(alias-macro with-lock-held bordeaux-threads:with-lock-held)
(alias-function condition-wait bordeaux-threads:condition-wait)
(alias-function make-lock bordeaux-threads:make-lock)
(alias-function make-condition-variable
                bordeaux-threads:make-condition-variable)
(alias-function current-thread bordeaux-threads:current-thread)
(alias-function destroy-thread bordeaux-threads:destroy-thread)
#+lparallel.with-green-threads
(alias-function thread-yield bordeaux-threads:thread-yield)

#+clisp
(defmacro with-abort-restart (&body body)
  `(restart-case 
       (progn ,@body)
     (abort ()
       :report "Abort thread.")))

#-clisp
(defmacro with-abort-restart (&body body)
  `(progn ,@body))

(defmacro with-thread ((&key bindings name) &body body)
  `(let1 *default-special-bindings* ,bindings
     (make-thread (lambda ()
                    (with-abort-restart
                      ,@body))
                  :name ,name)))

(defmacro with-lock-predicate/no-wait (lock predicate &body body)
  ;; predicate intentionally evaluated twice
  (with-gensyms (lock-var)
    `(when ,predicate
       (let1 ,lock-var ,lock
         (when (acquire-lock ,lock-var nil)
           (unwind-protect
                (when ,predicate
                  ,@body)
             (release-lock ,lock-var)))))))

(defmacro with-lock-predicate/wait (lock predicate &body body)
  ;; predicate intentionally evaluated twice
  `(when ,predicate
     (with-lock-held (,lock)
       (when ,predicate
         ,@body))))

(defmacro define-locking-fn/base (name args arg-types return-type
                                  lock-reader
                                  defun/no-lock
                                  arg-types/no-lock return-type/no-lock
                                  &body body)
  (let1 name/no-lock (symbolicate name '#:/no-lock)
    `(progn
       (,defun/no-lock ,name/no-lock ,args
         ,@(unsplice arg-types/no-lock)
         ,@(unsplice return-type/no-lock)
         ,@body)
       (defun/type ,name ,args ,arg-types ,return-type
         (declare #.*normal-optimize*)
         (with-lock-held ((,lock-reader ,(car (last args))))
           (,name/no-lock ,@args))))))

(defmacro define-locking-fn (name args arg-types return-type lock &body body)
  `(define-locking-fn/base
       ,name ,args ,arg-types ,return-type ,lock
       defun/type ,arg-types ,return-type
     (declare #.*normal-optimize*)
     ,@body))

(defmacro define-simple-locking-fn (name args arg-types return-type lock
                                    &body body)
  `(define-locking-fn/base
       ,name ,args ,arg-types ,return-type ,lock
       defun/inline nil nil
     (declare #.*normal-optimize*)
     ,@body))

#+lparallel.with-green-threads
(defun condition-notify-and-yield (cvar)
  (condition-notify cvar)
  (thread-yield))

#-lparallel.with-green-threads
(alias-function condition-notify-and-yield condition-notify)
