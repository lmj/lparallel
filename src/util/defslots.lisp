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

(defun plist-keys (plist)
  (loop for x in plist by #'cddr
        collect x))

(defun plist-values-for-key (plist target-key)
  (loop for (key value) on plist by #'cddr
        when (eq key target-key)
        collect value))

(defun parse-defslots (supers slots options)
  (unless (<= (length supers) 1)
    (error "More than one superclass specified in DEFSLOTS: ~s" supers))
  (unless (<= (length options) 1)
    (error "Too many options in DEFSLOTS: ~{~s ~}" options))
  (unless (or (null options)
              (eq (caar options) :documentation))
    (error "Option ~s in DEFSLOTS is not :DOCUMENTATION" (caar options)))
  (loop with allowed = '(:initform :type :reader)
        for (nil . plist) in slots
        for keys = (plist-keys plist)
        do (let ((diff (set-difference keys allowed)))
             (unless (null diff)
               (error "Slot option ~s in DEFSLOTS is not one of ~s"
                      (first diff) allowed)))))

(defun defslots-names (name)
  (values (symbolicate '#:make- name '#:-instance)
          (symbolicate '#:with- name '#:-slots)
          (symbolicate/no-intern '#:%%%%. name '#:.)
          (make-symbol (package-name *package*))))

#-lparallel.with-debug
(progn
  (defmacro define-slots-macrolet (package conc-name entries instance
                                   &body body)
    `(symbol-macrolet
         ,(loop for entry in entries
                for (name slot) = (if (consp entry) entry `(,entry ,entry))
                for accessor = (symbolicate/package package conc-name slot)
                collect `(,name (,accessor ,instance)))
       ,@body))

  (defmacro define-with-slots-macro (name package conc-name)
    `(defmacro/once ,name (slots &once instance &body body)
       `(define-slots-macrolet ,',package ,',conc-name ,slots ,instance
          ,@body)))

  (defmacro define-struct (name supers slots options conc-name constructor)
    `(defstruct (,name (:conc-name ,conc-name)
                       (:constructor ,constructor)
                       ,@(unsplice (when supers `(:include ,(first supers)))))
       ,@(unsplice (getf (first options) :documentation))
       ,@(loop for (slot-name . plist) in slots
               for initform = (getf plist :initform
                                    `(error "slot ~a in ~a not initialized"
                                            ',slot-name ',name))
               for type = (getf plist :type)
               collect `(,slot-name ,initform ,@(when type `(:type ,type))))))

  (defmacro define-reader (public private type struct)
    `(progn
       (declaim (ftype (function (,struct) (values ,(or type t) &optional))
                       ,public))
       (alias-function ,public ,private)))

  (defmacro define-readers (struct conc-name slots)
    `(progn
       ,@(loop for (slot-name . plist) in slots
               for private = (symbolicate conc-name slot-name)
               for type = (getf plist :type)
               append (loop for public in (plist-values-for-key plist :reader)
                            collect `(define-reader
                                         ,public ,private ,type ,struct)))))

  (defmacro %defslots (name supers slots options)
    (multiple-value-bind (constructor slots-macro-name conc-name package)
        (defslots-names name)
      `(progn
         (define-struct ,name ,supers ,slots ,options ,conc-name ,constructor)
         (define-with-slots-macro ,slots-macro-name ,package ,conc-name)
         (define-readers ,name ,conc-name ,slots)
         ',name))))

#+lparallel.with-debug
(defmacro %defslots (name supers slots options)
  (multiple-value-bind (constructor slots-macro-name) (defslots-names name)
    `(progn
       (defclass ,name ,supers
         ,(loop for slot in (copy-list slots)
                for slot-name = (first slot)
                for initarg = (intern (symbol-name slot-name) 'keyword)
                collect `(,@slot :initarg ,initarg))
         ,@options)
       (defmacro ,slots-macro-name (slot-names instance &body body)
         `(with-slots ,slot-names ,instance ,@body))
       (defun ,constructor (&rest args)
         (apply #'make-instance ',name args))
       ',name)))

(defmacro defslots (name supers slots &rest options)
  "Define a thing with slots.

A `defslots' form may expand to either a `defclass' form or a
`defstruct' form. Thou art foolish to depend upon either.

The syntax of `defslots' matches that of `defclass' with the following
restrictions: at most one superclass is permitted; `:initform',
`:type', and `:reader', are the only slot options allowed;
`:documentation' is the only class option allowed.

`(defslots foo ...)' defines the functions `make-foo-instance' and
`with-foo-slots' which are like `make-instance' and `with-slots'
respectively. `make-foo-instance' takes keyword arguments
corresponding to slots of the same name.

All slots must be initialized when an instance is created, else an
error will be signaled."
  (setf slots (mapcar #'ensure-list slots))
  (parse-defslots supers slots options)
  `(%defslots ,name ,supers ,slots ,options))
