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

(def-suite :lparallel)
(in-suite :lparallel)

(defun execute ()
  (run!))

(defun debug-execute ()
  (debug!))

(defmacro with-new-kernel ((&rest args) &body body)
  `(let1 *kernel* (make-kernel ,@args)
     (unwind-protect
          (progn ,@body)
       (end-kernel :wait t))))

(defmacro lp-base-test (name &body body)
  `(progn
     (test #-lparallel-test.with-precompile ,name
           #+lparallel-test.with-precompile (,name :compile-at :definition-time)
       (format t "~&~a~%" ',name)
       (finish-output)
       ,@body)
     (defun ,name ()
       (debug! ',name))))

(defmacro lp-test (name &body body)
  (with-gensyms (body-fn n)
    `(lp-base-test ,name
       (let1 *random-state* (make-random-state t)
         (dolist (,n '(1 2 4 8 16))
           (flet ((,body-fn () ,@body))
             (with-new-kernel (,n :spin-count 0)
               (,body-fn))
             (with-new-kernel (,n :spin-count 2000)
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
          #+sbcl (let1 term (find-symbol (string '#:terminate-thread)
                                         'sb-thread)
                   (if (and term (find term restarts))
                       (invoke-restart term)
                       (fail)))))))

(defmacro with-thread-count-check (&body body)
  (with-gensyms (old-thread-count)
    `(progn
       (sleep 0.2)
       (let1 ,old-thread-count (length (bordeaux-threads:all-threads))
         ,@body
         (sleep 0.2)
         (is (eql ,old-thread-count
                  (length (bordeaux-threads:all-threads))))))))

(defparameter *nil* nil)
(defun infinite-loop () (loop :until *nil*))
