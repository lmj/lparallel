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

(in-package #:lparallel.kernel)

(defslots timeout ()
  ((canceled-result)
   (thread)
   (lock :initform (make-lock))))

(defun submit-timeout (channel timeout-seconds timeout-result)
  "Effectively equivalent to

  (submit-task channel (lambda () (sleep timeout-seconds) timeout-result))

The difference is that `submit-timeout' does not occupy a worker
thread.

A timeout object is returned, which may be passed to `cancel-timeout'.

`submit-timeout' and `cancel-timeout' are deprecated; use the new
`:timeout' option in `try-receive-result'."
  (let ((timeout (make-timeout-instance
                  :canceled-result 'not-canceled :thread nil))
        (pushedp nil))
    (with-channel-slots (queue) channel
      (with-timeout-slots (canceled-result thread lock) timeout
        (macrolet ((push-result (form)
                     ;; Ensure that only one result is pushed.
                     ;;
                     ;; We must check the canceled result inside the
                     ;; lock, so delay evaluation via macrolet.
                     `(with-lock-predicate/wait lock (not pushedp)
                        (push-queue ,form queue)
                        (setf pushedp t))))
          (setf thread (with-thread (:name "lparallel-timeout")
                         (unwind-protect/ext
                          :main  (sleep timeout-seconds)
                          :abort (push-result
                                  (if (eq canceled-result 'not-canceled)
                                      (wrap-error 'task-killed-error)
                                      canceled-result)))
                         (push-result timeout-result))))))
    timeout))

#-lparallel.without-kill
(defun cancel-timeout (timeout timeout-result)
  "Attempt to cancel a timeout. If successful, the channel passed to
`submit-timeout' will receive `timeout-result'.

At most one call to `cancel-timeout' will succeed; others will be
ignored. If the timeout has expired on its own then `cancel-timeout'
will have no effect.

`cancel-timeout' is not available in ABCL.

`submit-timeout' and `cancel-timeout' are deprecated; use the new
`:timeout' option in `try-receive-result'."
  (with-timeout-slots (canceled-result thread lock) timeout
    ;; ensure that only one cancel succeeds
    (with-lock-predicate/wait lock (eq canceled-result 'not-canceled)
      (setf canceled-result timeout-result)
      (destroy-thread thread)))
  nil)

(defun deprecated-timeout ()
  (simple-style-warning
   "`submit-timeout' and `cancel-timeout' are deprecated; use the new~%~
   `:timeout' option in `try-receive-result'."))

(define-compiler-macro submit-timeout (&whole whole &rest args)
  (declare (ignore args))
  (deprecated-timeout)
  whole)

(define-compiler-macro cancel-timeout (&whole whole &rest args)
  (declare (ignore args))
  (deprecated-timeout)
  whole)
