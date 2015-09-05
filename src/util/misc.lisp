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

(defmacro alias-function (alias orig)
  `(progn
     (setf (symbol-function ',alias) #',orig)
     (define-compiler-macro ,alias (&rest args)
       `(,',orig ,@args))
     ',alias))

(defmacro alias-macro (alias orig)
  `(progn
     (setf (macro-function ',alias) (macro-function ',orig))
     ',alias))

(defun unsplice (form)
  (if form (list form) nil))

(defun symbolicate/package (package &rest string-designators)
  "Concatenate `string-designators' then intern the result into `package'."
  (let ((*package* (find-package package)))
    (apply #'symbolicate string-designators)))

(defun symbolicate/no-intern (&rest string-designators)
  "Concatenate `string-designators' then make-symbol the result."
  (format-symbol nil "~{~a~}" string-designators))

(defmacro with-parsed-body ((body declares &optional docstring) &body own-body)
  "Pop docstring and declarations off `body' and assign them to the
variables `docstring' and `declares' respectively. If `docstring' is
not present then no docstring is parsed."
  (if docstring
      `(multiple-value-bind (,body ,declares ,docstring)
           (parse-body ,body :documentation t)
         ,@own-body)
      `(multiple-value-bind (,body ,declares) (parse-body ,body)
         ,@own-body)))

(declaim (inline to-boolean))
(defun to-boolean (x)
  (if x t nil))

(defun interact (&rest prompt)
  "Read from user and eval."
  (apply #'format *query-io* prompt)
  (finish-output *query-io*)
  (multiple-value-list (eval (read *query-io*))))

(defmacro repeat (n &body body)
  `(loop repeat ,n do (progn ,@body)))

(defmacro dosequence ((var sequence &optional return) &body body)
  (with-gensyms (body-fn)
    `(block nil
       (flet ((,body-fn (,var) ,@body))
         (declare (dynamic-extent #',body-fn))
         (map nil #',body-fn ,sequence)
         ,@(unsplice (when return
                       `(let ((,var nil))
                          (declare (ignorable ,var))
                          ,return)))))))

(defmacro unwind-protect/ext (&key prepare main cleanup abort)
  "Extended `unwind-protect'.

`prepare' : executed first, outside of `unwind-protect'
`main'    : protected form
`cleanup' : cleanup form
`abort'   : executed if `main' does not finish
"
  (with-gensyms (finishedp)
    `(progn
       ,@(unsplice prepare)
       ,(cond ((and main cleanup abort)
               `(let ((,finishedp nil))
                  (declare (type boolean ,finishedp))
                  (unwind-protect
                       (prog1 ,main  ; m-v-prog1 in real life
                         (setf ,finishedp t))
                    (if ,finishedp
                        ,cleanup
                        (unwind-protect ,abort ,cleanup)))))
              ((and main cleanup)
               `(unwind-protect ,main ,cleanup))
              ((and main abort)
               `(let ((,finishedp nil))
                  (declare (type boolean ,finishedp))
                  (unwind-protect
                       (prog1 ,main
                         (setf ,finishedp t))
                    (when (not ,finishedp)
                      ,abort))))
              (main main)
              (cleanup `(progn ,cleanup nil))
              (abort nil)
              (t nil)))))

(deftype index () 'array-index)

(alias-function partial-apply curry)
