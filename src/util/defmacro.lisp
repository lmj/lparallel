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

(in-package #:lparallel.util)

(defmacro defmacro/once (name params &body body)
  "Like `defmacro' except that params which are immediately preceded
by `&once' are passed to a `once-only' call which surrounds `body'."
  (labels ((once-keyword-p (obj)
             (and (symbolp obj) (equalp (symbol-name obj) "&once")))
           (remove-once-keywords (params)
             (mapcar (lambda (x) (if (consp x) (remove-once-keywords x) x))
                     (remove-if #'once-keyword-p params)))
           (find-once-params (params)
             (mapcon (lambda (x)
                       (cond ((consp (first x))
                              (find-once-params (first x)))
                             ((once-keyword-p (first x))
                              (unless (and (cdr x) (atom (cadr x)))
                                (error "`&once' without parameter in ~a" name))
                              (list (second x)))
                             (t
                              nil)))
                     params)))
    (with-parsed-body (preamble body)
      `(defmacro ,name ,(remove-once-keywords params)
         ,@preamble
         (once-only ,(find-once-params params)
           ,@body)))))
