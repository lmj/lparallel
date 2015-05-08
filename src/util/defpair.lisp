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

(defmacro defpair (name supers (a b) &optional doc)
  "Define a cons type using defclass syntax.

Exactly two slots and zero superclasses must be given.

Available slot options are: `:initform', `:type', `:reader'.

A deftype for `name' is defined.

`(defpair foo ...)' defines the function `make-foo-instance' which
takes keyword arguments corresponding to slots of the same name.

All slots must be initialized when an instance is created, else an
error will be signaled."
  (unless (null supers)
    (error "Non-empty superclass list ~a in DEFPAIR ~a" supers name))
  (when doc
    (unless (and (consp doc) (eq :documentation (car doc)))
      (error "Expected `:documentation' option in DEFPAIR, got ~a" doc)))
  (setf a (ensure-list a))
  (setf b (ensure-list b))
  (labels ((slot-name  (slot) (car slot))
           (slot-props (slot) (cdr slot))
           (slot-type  (slot) (or (getf (slot-props slot) :type) t)))
    (when (eq (slot-name a) (slot-name b))
      (error "Multiple slots named ~a in DEFPAIR ~a" (slot-name a) name))
    (dolist (slot (list a b))
      (unless (slot-name slot)
        (error "empty slot in ~a" name))
      (when (slot-props slot)
        (let ((diff (set-difference (plist-keys (slot-props slot))
                                    '(:initform :type :reader))))
          (unless (null diff)
            (error "Invalid slot option~p in DEFPAIR: ~{~a^ ~}"
                   (length diff) diff)))))
    `(progn
       (deftype ,name () `(cons ,',(slot-type a)
                                ,',(slot-type b)))
       (alias-function ,(symbolicate '#:make- name '#:-instance) cons)
       ,@(loop for slot in `(,a ,b)
               for fn in '(car cdr)
               for readers = (plist-values-for-key (slot-props slot) :reader)
               when readers
               collect `(progn
                          ,@(loop for reader in readers
                                  collect `(alias-function ,reader ,fn)))))))
