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

(in-package #:lparallel.cognate)

(defmacro with-forms-submitted (forms &body body)
  `(with-submit-cancelable
     ,@(loop for form in forms
             collect `(submit-cancelable (lambda () ,form)))
     ,@body))

(defmacro pand (&rest forms)
  "Parallel version of `and'. Forms in `forms' may be executed in
parallel, though not necessarily at the same time. If all forms
evaluate to true, then the result of any form may be returned."
  (with-gensyms (done result next-result)
    `(block ,done
       (with-forms-submitted ,forms
         (let ((,result nil))
           (receive-cancelables ,next-result
             (unless (setf ,result ,next-result)
               (return-from ,done nil)))
           ,result)))))

(defmacro por (&rest forms)
  "Parallel version of `or'. Forms in `forms' may be executed in
parallel, though not necessarily at the same time. Any form which
evaluates to non-nil may be returned."
  (with-gensyms (done result)
    `(block ,done
       (with-forms-submitted ,forms
         (receive-cancelables ,result
           (when ,result
             (return-from ,done ,result)))
         nil))))
