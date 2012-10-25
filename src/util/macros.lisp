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

(deftype index () `(integer 0 ,array-dimension-limit))

(defmacro let1 (var value &body body)
  "Make a single `let' binding, heroically saving three columns."
  `(let ((,var ,value))
     ,@body))

(defmacro while (test &body body)
  `(loop :while ,test :do (progn ,@body)))

(defmacro until (test &body body)
  `(loop :until ,test :do (progn ,@body)))

(defmacro repeat (n &body body)
  `(loop :repeat ,n :do (progn ,@body)))

(defmacro when-let ((var test) &body body)
  `(let1 ,var ,test
     (when ,var ,@body)))

(defmacro while-let ((var test) &body body)
  `(loop (let1 ,var ,test
           (if ,var
               (progn ,@body)
               (return)))))

(defmacro dosequence ((var sequence &optional return) &body body)
  `(block nil
     (map nil (lambda (,var) ,@body) ,sequence)
     ,(if return
          `(let1 ,var nil
             (declare (ignorable ,var))
             ,return)
          nil)))

(defmacro rebind (vars &body body)
  `(let ,(loop
            :for var :in vars
            :collect `(,var ,var))
     ,@body))

(defmacro unwind-protect/ext (&key prepare main cleanup abort)
  "Extended `unwind-protect'.

`prepare' : executed first, outside of `unwind-protect'
`main'    : protected form
`cleanup' : cleanup form
`abort'   : executed if `main' does not finish
"
  (with-gensyms (finishedp cleanup-fn)
    `(progn
       ,@(unsplice prepare)
       ,(if main
            (if abort
                `(let1 ,finishedp nil
                   (declare (boolean ,finishedp)
                            (dynamic-extent ,finishedp))
                   (unwind-protect
                        (prog1               ; m-v-prog1 in real life
                            ,main
                          (setf ,finishedp t))
                     ,(if cleanup
                          `(flet ((,cleanup-fn () ,cleanup))
                             (declare (dynamic-extent #',cleanup-fn))
                             (if ,finishedp
                                 (,cleanup-fn)
                                 (unwind-protect
                                      ,abort
                                   (,cleanup-fn))))
                          `(unless ,finishedp
                             ,abort))))
                (if cleanup
                    `(unwind-protect
                          ,main
                       ,cleanup)
                    main))
            `(progn ,cleanup nil)))))

;;;; alias

(defun doc-deprecate (deprecated preferred doc-type)
  (setf (documentation deprecated doc-type)
        (format nil "Deprecated. Use `~a' instead."
                (string-downcase (string preferred)))))

(defmacro alias-function (alias orig &key deprecate)
  (check-type deprecate boolean)
  `(progn
     (setf (symbol-function ',alias) #',orig)
     (define-compiler-macro ,alias (&rest args)
       `(,',orig ,@args))
     ,@(when deprecate `((doc-deprecate ',alias ',orig 'function)))
     ',alias))

(defmacro alias-macro (alias orig &key deprecate)
  (check-type deprecate boolean)
  `(progn
     (setf (macro-function ',alias) (macro-function ',orig))
     ,@(when deprecate `((doc-deprecate ',alias ',orig 'function)))
     ',alias))

(defmacro alias-special (alias orig &key deprecate)
  (check-type deprecate boolean)
  `(progn
     (define-symbol-macro ,alias ,orig)
     ,@(when deprecate `((doc-deprecate ',alias ',orig 'variable)))
     ',alias))

(defmacro import-now (&rest symbols)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (import ',symbols)))
