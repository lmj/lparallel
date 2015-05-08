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

(defpackage #:lparallel.thread-util
  (:documentation
   "(private) Thread utilities.")
  (:use #:cl
        #:lparallel.util)
  (:export #:with-thread
           #:with-lock-predicate/wait
           #:with-lock-predicate/no-wait
           #:condition-notify
           #:cas
           #:make-spin-lock
           #:with-spin-lock-held)
  (:export #:make-lock
           #:make-condition-variable
           #:with-lock-held
           #:condition-wait
           #:destroy-thread
           #:current-thread)
  #+lparallel.with-green-threads
  (:export #:thread-yield)
  (:import-from #:bordeaux-threads
                #:*default-special-bindings*
                #:make-thread
                #:make-condition-variable
                #:current-thread
                #:destroy-thread
                #:make-lock
                #:acquire-lock
                #:release-lock
                #:with-lock-held)
  #+lparallel.with-green-threads
  (:import-from #:bordeaux-threads
                #:thread-yield))

(in-package #:lparallel.thread-util)

;;;; condition-wait

;;; Check for timeout parameter in bordeaux-threads:condition-wait.
(eval-when (:compile-toplevel :execute)
  ;; use special to defeat compiler analysis
  (defparameter *condition-wait* #'bordeaux-threads:condition-wait)

  (flet ((has-condition-wait-timeout-p ()
           (let* ((lock (bordeaux-threads:make-lock))
                  (cvar (bordeaux-threads:make-condition-variable))
                  (args `(,cvar ,lock :timeout 0.001)))
             (bordeaux-threads:with-lock-held (lock)
               (ignore-errors
                 (apply *condition-wait* args)
                 t)))))
    (unless (has-condition-wait-timeout-p)
      (pushnew :lparallel.without-bordeaux-threads-condition-wait-timeout
               *features*))))

#+lparallel.without-bordeaux-threads-condition-wait-timeout
(progn
  (eval-when (:load-toplevel)
    (pushnew :lparallel.without-bordeaux-threads-condition-wait-timeout
             *features*))

  (defun condition-wait (cvar lock &key timeout)
    (if timeout
        (error "Timeout option is not available in this version of ~
                bordeaux-threads.")
        (bordeaux-threads:condition-wait cvar lock))))

#-lparallel.without-bordeaux-threads-condition-wait-timeout
(alias-function condition-wait bordeaux-threads:condition-wait)

;;;; condition-notify

#+lparallel.with-green-threads
(defun condition-notify (cvar)
  (bordeaux-threads:condition-notify cvar)
  (thread-yield))

#-lparallel.with-green-threads
(alias-function condition-notify bordeaux-threads:condition-notify)

;;;; cas and spin-lock

#+lparallel.with-cas
(progn
  (defmacro cas (place old new &environment env)
    (declare (ignorable env))
    (check-type old symbol)
    ;; macroexpand is needed for sbcl-1.0.53 and older
    #+sbcl `(eq ,old (sb-ext:compare-and-swap ,(macroexpand place env)
                                              ,old ,new))
    #+ccl `(ccl::conditional-store ,place ,old ,new)
    #+lispworks `(sys:compare-and-swap ,place ,old ,new))

  #-(or sbcl ccl lispworks)
  (error "cas not defined")

  (defun make-spin-lock ()
    nil)

  (defmacro/once with-spin-lock-held (((access &once container)) &body body)
    `(locally (declare #.*full-optimize*)
       (unwind-protect/ext
        :prepare (loop until (cas (,access ,container) nil t))
        :main (progn ,@body)
        :cleanup (setf (,access ,container) nil)))))

#-lparallel.with-cas
(progn
  (defun make-spin-lock ()
    (make-lock))

  (defmacro with-spin-lock-held (((access container)) &body body)
    `(with-lock-held ((,access ,container))
       ,@body)))

;;;; general-purpose utilities

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
  `(let ((*default-special-bindings* ,bindings))
     (make-thread (lambda ()
                    (with-abort-restart
                      ,@body))
                  :name ,name)))

(defmacro with-lock-predicate/no-wait (lock predicate &body body)
  ;; predicate intentionally evaluated twice
  (with-gensyms (lock-var)
    `(when ,predicate
       (let ((,lock-var ,lock))
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

;;;; special-purpose utilities

(defun/inline get-real-time-in-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun %time-remaining (start timeout)
  (- timeout
     (- (get-real-time-in-seconds) start)))

(defmacro/once with-countdown ((&once time) &body body)
  (with-gensyms (start)
    `(let ((,start (get-real-time-in-seconds)))
       (flet ((time-remaining () (%time-remaining ,start ,time)))
         (declare (inline time-remaining))
         ,@body))))

(defmacro define-locking-fn/base (name args arg-types return-type
                                  lock-reader
                                  defun/no-lock
                                  arg-types/no-lock return-type/no-lock
                                  &body body)
  (let ((name/no-lock (symbolicate name '#:/no-lock)))
    `(progn
       (,defun/no-lock ,name/no-lock ,args
         ,@(unsplice arg-types/no-lock)
         ,@(unsplice return-type/no-lock)
         ,@body)
       (defun/type ,name ,args ,arg-types ,return-type
         (declare #.*full-optimize*)
         (with-lock-held ((,lock-reader ,(car (last args))))
           (,name/no-lock ,@args))))))

(defmacro define-locking-fn (name args arg-types return-type lock &body body)
  `(define-locking-fn/base
       ,name ,args ,arg-types ,return-type ,lock
       defun/type ,arg-types ,return-type
     (declare #.*full-optimize*)
     ,@body))

(defmacro define-simple-locking-fn (name args arg-types return-type lock
                                    &body body)
  `(define-locking-fn/base
       ,name ,args ,arg-types ,return-type ,lock
       defun/inline nil nil
     (declare #.*full-optimize*)
     ,@body))
