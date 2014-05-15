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

(defpackage #:lparallel.counter
  (:documentation
   "(private) Atomic counter.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util)
  (:export #:counter
           #:make-counter
           #:inc-counter
           #:dec-counter
           #:counter-value))

(in-package #:lparallel.counter)

;;; Atomic counters modeled after SBCL, i.e., operations return the
;;; original value.

#+sbcl
(progn
  ;; try to avoid using sb-ext:word since it is newish
  (deftype counter-value ()
    #+(or x86-64 x86)
    '(unsigned-byte #+x86-64 64 #+x86 32)
    #-(or x86-64 x86)
    'sb-ext:word)

  (defstruct (counter (:constructor make-counter (&optional value)))
    (value 0 :type counter-value))

  (defmacro define-counter-fn (name op)
    `(defun/inline ,name (counter)
       (,op (counter-value counter))))

  (define-counter-fn inc-counter sb-ext:atomic-incf)
  (define-counter-fn dec-counter sb-ext:atomic-decf))

#+(or ccl lispworks)
(progn
  (deftype counter () 'cons)

  (defun make-counter (&optional (value 0))
    (cons value nil))

  (alias-function counter-value car)

  (defmacro define-counter-fn (name op adjust)
    `(defun/inline ,name (counter)
       (,adjust (,op (car counter)))))

  ;;; Strangely, Clozure does advertise these atomic operations but does
  ;;; not export the symbols.

  (define-counter-fn inc-counter #+ccl       ccl::atomic-incf
                                 #+lispworks system:atomic-incf
                                 1-)
  (define-counter-fn dec-counter #+ccl       ccl::atomic-decf
                                 #+lispworks system:atomic-decf
                                 1+))

#-(or sbcl ccl lispworks)
(progn
  (defslots counter ()
    ((value :reader counter-value)
     (lock  :reader lock          :initform (make-lock))))

  (defun make-counter (&optional (value 0))
    (make-counter-instance :value value))

  (defmacro define-counter-fn (name op adjust)
    `(defun/inline ,name (counter)
       (with-counter-slots (value lock) counter
         (,adjust (with-lock-held (lock)
                    (,op value))))))

  (define-counter-fn inc-counter incf 1-)
  (define-counter-fn dec-counter decf 1+))
