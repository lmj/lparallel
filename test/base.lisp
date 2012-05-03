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

(defmacro/once with-new-kernel ((&once worker-count
                                 &rest args
                                 &key bindings worker-context name)
                                &body body)
  (declare (ignore bindings worker-context name))
  `(let1 *kernel* (make-kernel ,worker-count ,@args)
     (unwind-protect (progn ,@body)
       (end-kernel))))

(defmacro lp-base-test (name &body body)
  `(progn
     (test #+lparallel.with-debug (,name :compile-at :definition-time)
           #-lparallel.with-debug ,name
       (format t "~&~a~%" ',name)
       ,@body)
     (defun ,name ()
       (debug! ',name))))

(defmacro lp-test (name &body body)
  (with-gensyms (n)
    `(lp-base-test ,name
       (let1 *random-state* (make-random-state t)
         (dolist (,n '(1 2 4 8 16))
           (with-new-kernel (,n)
             ,@body))))))

(define-condition client-error (error) ())
(define-condition foo-error (error) ())

(defparameter *memo* nil)

(defun extract-queue (queue)
  (loop
     :until (queue-empty-p queue)
     :collect (pop-queue queue)))

(defun peer-queue (queue)
  (loop
     :for elem :in (extract-queue queue)
     :do (push-queue elem queue)
     :collect elem))

(defun invoke-abort-thread ()
  (invoke-restart #-sbcl 'abort
                  #+sbcl 'sb-thread:terminate-thread))

(defmacro with-thread-count-check (&body body)
  (with-gensyms (old-thread-count)
    `(progn
       (sleep 0.2)
       (let1 ,old-thread-count (length (bordeaux-threads:all-threads))
         ,@body
         (sleep 0.2)
         (is (eql ,old-thread-count
                  (length (bordeaux-threads:all-threads))))))))
