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

(in-package #:lparallel.counter)

;;; 
;;; Strangely, Clozure does advertise these atomic operations but does
;;; not export the symbols.
;;; 

#+(or clozure lispworks)
(progn
  (deftype counter () '(cons fixnum null))

  (defun/inline make-counter (&optional (value 0))
    (declare (fixnum value))
    (the counter (cons value nil)))

  (alias-function counter-value car)

  (defmacro define-counter-fn (name op)
    `(defun/inline ,name (counter)
       (declare (type counter counter))
       (the fixnum (,op (car counter)))))
  
  (define-counter-fn inc-counter #+clozure   ccl::atomic-incf
                                 #+lispworks system:atomic-incf)
  (define-counter-fn dec-counter #+clozure   ccl::atomic-decf
                                 #+lispworks system:atomic-decf))

#+sbcl
(progn
  (deftype counter-value () '(unsigned-byte
                              #+x86-64 64
                              #-x86-64 32))

  (defstruct (counter (:constructor make-counter (&optional value)))
    (value 0 :type counter-value))
  
  (defmacro define-counter-fn (name op adjust)
    `(defun/inline ,name (counter)
       (declare (type counter counter))
       (,adjust (,op (counter-value counter)))))

  (define-counter-fn inc-counter sb-ext:atomic-incf 1+)
  (define-counter-fn dec-counter sb-ext:atomic-decf 1-))

#-(or clozure lispworks sbcl)
(progn
  (defslots counter ()
    ((value :reader counter-value :type fixnum)
     (lock  :reader lock          :initform (make-lock))))

  (defun/inline make-counter (&optional (value 0))
    (declare (fixnum value))
    (make-counter-instance :value value))

  (defmacro define-counter-fn (name op)
    `(define-locking-fn ,name (counter) (counter) fixnum lock
       (with-counter-slots (value) counter
         (,op value))))

  (define-counter-fn inc-counter incf)
  (define-counter-fn dec-counter decf))
