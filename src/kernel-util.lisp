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

(in-package #:lparallel.kernel-util)

(import-now lparallel.kernel::*worker*
            lparallel.kernel::steal-work
            lparallel.kernel::channel-kernel)

(defun steal-until-receive-result (channel worker fn)
  (declare #.*normal-optimize*)
  (loop
     (multiple-value-bind (result presentp) (try-receive-result channel)
       (when presentp
         (when fn
           (locally (declare (type function fn))
             (funcall fn result)))
         (return)))
     (steal-work (channel-kernel channel) worker)))

(defun receive-results (channel count fn)
  (declare #.*normal-optimize*)
  (let ((worker *worker*))
    (if worker
        (repeat count
          (steal-until-receive-result channel worker fn))
        (if fn
            (do-fast-receives (result channel count)
              (locally (declare (type function fn))
                (funcall fn result)))
            (do-fast-receives (result channel count)
              (declare (ignore result)))))))

(defmacro with-submit-counted (&body body)
  (with-gensyms (count channel)
    `(let ((,count   0)
           (,channel (make-channel)))
       (declare (fixnum ,count))
       (flet ((submit-counted (&rest args)
                (declare (dynamic-extent args))
                (apply #'submit-task ,channel args)
                (incf ,count))
              (receive-counted ()
                (receive-results ,channel ,count nil)))
         (declare (inline submit-counted receive-counted))
         ,@body))))

(defun indexing-wrapper (array index function args)
  (setf (aref array index) (apply function args)))

(defmacro/once with-submit-indexed (&once count &once array &body body)
  (with-gensyms (channel)
    `(let ((,channel (make-channel)))
       (flet ((submit-indexed (index function &rest args)
                (submit-task
                 ,channel #'indexing-wrapper ,array index function args))
              (receive-indexed ()
                (receive-results ,channel ,count nil)
                ,array))
         (declare (inline submit-indexed receive-indexed))
         ,@body))))

(defmacro with-submit-cancelable (&body body)
  (with-gensyms (canceledp channel count)
    `(let ((,canceledp nil)
           (,count     0)
           (,channel   (make-channel)))
       (flet ((submit-cancelable (fn &rest args)
                (submit-task ,channel
                             (lambda ()
                               (if ,canceledp
                                   'task-canceled
                                   (apply fn args))))
                (incf ,count)))
         (macrolet ((receive-cancelables (result &body body)
                      `(receive-results
                        ,',channel ,',count (lambda (,result) ,@body))))
           (unwind-protect (progn ,@body)
             (setf ,canceledp t)))))))
