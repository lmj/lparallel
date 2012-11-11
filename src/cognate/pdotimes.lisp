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

(defun %pdotimes (size parts fn)
  (check-type size fixnum)
  (when (plusp size)
    (let ((parts (get-parts-hint parts))
          (fn (ensure-function fn)))
      (with-parts size parts
        (with-submit-counted
          (while (next-part)
            (submit-counted
             (let ((part-offset (part-offset))
                   (part-size (part-size)))
               (declare #.*normal-optimize*)
               (declare (type fixnum part-offset part-size))
               (lambda ()
                 (let ((index part-offset)
                       (end (+ part-offset part-size)))
                   (declare (type fixnum index end))
                   (while (< index end)
                     (funcall fn index)
                     (incf index)))))))
          (receive-counted))))))

(defmacro/once pdotimes ((var &once count &optional result parts)
                         &body body)
  "Parallel version of `dotimes'.

The `parts' option divides the integer range into `parts' number of
parts. Default is (kernel-worker-count).

Unlike `dotimes', `pdotimes' does not define an implicit block named
nil."
  (with-parsed-body (nil declares body)
    `(progn
       (%pdotimes ,count ,parts (lambda (,var)
                                  ,@declares
                                  (tagbody ,@body)))
       (let ((,var (max ,count 0)))
         (declare (ignorable ,var))
         ,result))))
