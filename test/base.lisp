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

#-lparallel-test.with-simple-framework
(import-now eos:is eos:signals eos:test eos:run! eos:debug! eos:in-suite*)

#+lparallel-test.with-simple-framework
(import-now 1am:is 1am:signals 1am:test 1am:run! 1am:debug! 1am:in-suite*)

(in-suite* :lparallel)

(alias-function execute run!)

(defmacro with-new-kernel ((&rest args) &body body)
  `(let ((*kernel* (make-kernel ,@args)))
     (unwind-protect
          (progn ,@body)
       (end-kernel :wait t))))

(defmacro lp-base-test (name &body body)
  `(progn
     (test ,name
       (format t "~&~a~%" ',name)
       (finish-output)
       ,@body)
     #-lparallel-test.with-simple-framework
     (defun ,name ()
       (debug! ',name))))

(defvar *last-random-state* nil)

(defmacro lp-test (name &body body)
  (with-gensyms (body-fn n)
    `(lp-base-test ,name
       (let ((*random-state* (make-random-state t)))
         (setf *last-random-state* (make-random-state *random-state*))
         (dolist (,n '(1 2 4 8 16))
           (flet ((,body-fn () ,@body))
             (with-new-kernel (,n :spin-count 0)
               (,body-fn))
             #+lparallel.with-stealing-scheduler
             (with-new-kernel (,n :spin-count (random 5000))
               (,body-fn))))))))

(define-condition client-error (error) ())
(define-condition foo-error (error) ())

(defparameter *memo* nil)

(defun extract-queue (queue)
  (loop
     :until (queue-empty-p queue)
     :collect (pop-queue queue)))

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

(defmacro with-thread-count-check (&body body)
  (with-gensyms (old-thread-count)
    `(progn
       (sleep 0.2)
       (let ((,old-thread-count (thread-count)))
         ,@body
         (sleep 0.2)
         (is (eql ,old-thread-count (thread-count)))))))

(defparameter *nil* nil)
(defun infinite-loop () (loop :until *nil*))

(defmacro collect-n (n &body body)
  "Execute `body' `n' times, collecting the results into a list."
  `(loop :repeat ,n :collect (progn ,@body)))

(defun curry (fn &rest init-args)
  (lambda (&rest args)
    (multiple-value-call fn (values-list init-args) (values-list args))))

(defun make-random-list (size)
  (collect-n size (random 1.0)))

(defun make-random-vector (size)
  (map-into (make-array size) (lambda () (random 1.0))))
