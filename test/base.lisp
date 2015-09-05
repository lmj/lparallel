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

(in-package #:lparallel-test)

(define-condition client-error (error) ())
(define-condition foo-error (error) ())

(defparameter *memo* nil)
(defparameter *nil* nil)

(alias-function execute run)
(alias-macro base-test test)

(defun call-full-test (name body-fn)
  (dolist (n '(1 2 4 8 16))
    (with-temp-kernel (n :spin-count 0)
      (funcall body-fn))
    ;; kludge for checking :use-caller
    (when (search "defpun" (symbol-name name) :test #'equalp)
      (with-temp-kernel (n :spin-count (random 5000) :use-caller t)
        (funcall body-fn)))
    #+lparallel.with-stealing-scheduler
    (with-temp-kernel (n :spin-count (random 5000))
      (funcall body-fn))))

(defmacro full-test (name &body body)
  `(base-test ,name
     (call-full-test ',name (lambda () ,@body))))

(defun extract-queue (queue)
  (loop until (queue-empty-p queue)
        collect (pop-queue queue)))

(defun invoke-abort-thread ()
  (flet ((fail () (error "Can't find an abort-like restart in this CL!")))
    (let ((restarts (mapcar #'restart-name (compute-restarts))))
      (if (find 'abort restarts)
          (invoke-restart 'abort)
          #-sbcl (fail)
          #+sbcl (let ((term (find-symbol (string '#:terminate-thread)
                                          'sb-thread)))
                   (if (and term (find term restarts))
                       (invoke-restart term)
                       (fail)))))))

(defun thread-count ()
  ;; ccl can spontaneously lose the initial thread (issue #1042)
  #+ccl (count "Initial"
               (bordeaux-threads:all-threads)
               :key #'bordeaux-threads:thread-name
               :test-not #'string=)
  #-ccl (length (bordeaux-threads:all-threads)))

(defun call-with-thread-count-check (body-fn)
  (sleep 0.2)
  (let ((old-thread-count (thread-count)))
    (funcall body-fn)
    (sleep 0.2)
    (is (eql old-thread-count (thread-count)))))

(defmacro with-thread-count-check (&body body)
  `(call-with-thread-count-check (lambda () ,@body)))

(defun infinite-loop () (loop until *nil*))

(defmacro collect-n (n &body body)
  "Execute `body' `n' times, collecting the results into a list."
  `(loop repeat ,n collect (progn ,@body)))

(defun make-random-list (size)
  (collect-n size (random 1.0)))

(defun make-random-vector (size)
  (map-into (make-array size) (lambda () (random 1.0))))

(defun compile/muffled (&rest args)
  (handler-bind (((or warning
                      #+ecl c:compiler-note
                      #+sbcl sb-ext:compiler-note)
                  #'muffle-warning))
    (apply #'compile args)))
