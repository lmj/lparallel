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

;;; This yearns for defmethod, however there are outstanding issues
;;; with highly concurrent defmethod calls.
;;;
;;; The `queue' type is a chimera because a cons-based queue was
;;; measurably faster than a resizable vector queue even with
;;; pre-allocation.

(defpackage #:lparallel.queue
  (:documentation
   "Blocking FIFO queue for communication between threads.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.cons-queue
        #:lparallel.vector-queue)
  (:export #:queue
           #:make-queue
           #:push-queue    #:push-queue/no-lock
           #:pop-queue     #:pop-queue/no-lock
           #:peek-queue    #:peek-queue/no-lock
           #:queue-count   #:queue-count/no-lock
           #:queue-empty-p #:queue-empty-p/no-lock
           #:queue-full-p  #:queue-full-p/no-lock
           #:try-pop-queue #:try-pop-queue/no-lock
           #:with-locked-queue)
  (:import-from #:alexandria
                #:simple-style-warning))

(in-package #:lparallel.queue)

(deftype queue () '(or cons-queue vector-queue))

(defun %make-queue (&key fixed-capacity initial-contents)
  (if fixed-capacity
      (make-vector-queue fixed-capacity :initial-contents initial-contents)
      (make-cons-queue :initial-contents initial-contents)))

(defun make-queue (&rest args)
  (apply #'%make-queue (if (= 1 (length args))
                           nil
                           args)))

(define-compiler-macro make-queue (&whole whole &rest args)
  (when (= 1 (length args))
    (simple-style-warning
     "Calling `make-queue' with one argument is deprecated.~%~
      Pass no arguments instead."))
  whole)

(defun call-with-locked-cons-queue (fn queue)
  (with-locked-cons-queue queue
    (funcall fn)))

(defun call-with-locked-vector-queue (fn queue)
  (with-locked-vector-queue queue
    (funcall fn)))

(defmacro with-locked-queue (queue &body body)
  `(call-with-locked-queue (lambda () ,@body) ,queue))

(defun/inline cons-queue-full-p (queue)
  (declare (ignore queue))
  nil)

(defun/inline cons-queue-full-p/no-lock (queue)
  (declare (ignore queue))
  nil)

(defmacro define-queue-fn (name params cons-name vector-name)
  `(defun ,name ,params
     (declare #.*normal-optimize*)
     (typecase ,(car (last params))
       (cons-queue (,cons-name ,@params))
       (vector-queue (,vector-name ,@params))
       (otherwise (error 'type-error
                         :datum ,(car (last params))
                         :expected-type 'queue)))))

(define-queue-fn push-queue (object queue)
  push-cons-queue
  push-vector-queue)

(define-queue-fn push-queue/no-lock (object queue)
  push-cons-queue/no-lock
  push-vector-queue/no-lock)

(define-queue-fn pop-queue (queue)
  pop-cons-queue
  pop-vector-queue)

(define-queue-fn pop-queue/no-lock (queue)
  pop-cons-queue/no-lock
  pop-vector-queue/no-lock)

(define-queue-fn peek-queue (queue)
  peek-cons-queue
  peek-vector-queue)

(define-queue-fn peek-queue/no-lock (queue)
  peek-cons-queue/no-lock
  peek-vector-queue/no-lock)

(define-queue-fn queue-count (queue)
  cons-queue-count
  vector-queue-count)

(define-queue-fn queue-count/no-lock (queue)
  cons-queue-count/no-lock
  vector-queue-count/no-lock)

(define-queue-fn queue-empty-p (queue)
  cons-queue-empty-p
  vector-queue-empty-p)

(define-queue-fn queue-empty-p/no-lock (queue)
  cons-queue-empty-p/no-lock
  vector-queue-empty-p/no-lock)

(define-queue-fn queue-full-p (queue)
  cons-queue-full-p
  vector-queue-full-p)

(define-queue-fn queue-full-p/no-lock (queue)
  cons-queue-full-p/no-lock
  vector-queue-full-p/no-lock)

(defmacro define-try-pop-queue (name cons-name vector-name)
  `(defun ,name (queue &key timeout)
     (declare #.*normal-optimize*)
     (unless timeout
       (setf timeout 0))
     (typecase queue
       (cons-queue (,cons-name queue timeout))
       (vector-queue (,vector-name queue timeout))
       (otherwise (error 'type-error
                         :datum queue
                         :expected-type 'queue)))))

(define-try-pop-queue try-pop-queue
  try-pop-cons-queue
  try-pop-vector-queue)

(define-try-pop-queue try-pop-queue/no-lock
  try-pop-cons-queue/no-lock
  try-pop-vector-queue/no-lock)

(define-queue-fn call-with-locked-queue (fn queue)
  call-with-locked-cons-queue
  call-with-locked-vector-queue)

;;;; doc

(setf (documentation 'make-queue 'function)
"Create a queue.

The queue contents may be initialized with the keyword argument
`initial-contents'.

By default there is no limit on the queue capacity. Passing a
`fixed-capacity' keyword argument limits the capacity to the value
passed. `push-queue' will block for a full fixed-capacity queue.")

(setf (documentation 'peek-queue 'function)
"If `queue' is non-empty, return (values element t) where `element' is
the frontmost element of `queue'.

If `queue' is empty, return (values nil nil).")

(setf (documentation 'push-queue 'function)
"Push `object' onto the back of `queue'.")

(setf (documentation 'pop-queue 'function)
 "Remove the frontmost element from `queue' and return it.

If `queue' is empty, block until an element is available.")

(setf (documentation 'try-pop-queue 'function)
"If `queue' is non-empty, remove the frontmost element from `queue'
and return (values element t) where `element' is the element removed.

If `queue' is empty and `timeout' is given, then wait up to `timeout'
seconds for the queue to become non-empty.

If `queue' is empty and the timeout has expired, or if `queue' is
empty and no `timeout' was given, return (values nil nil).

Providing a nil or non-positive value of `timeout' is equivalent to
providing no timeout.")

(setf (documentation 'queue-count 'function)
"Return the number of elements in `queue'.")

(setf (documentation 'queue-empty-p 'function)
"Return true if `queue' is empty, otherwise return false.")

(setf (documentation 'queue-full-p 'function)
"Return true if `queue' is full, otherwise return false.")

(setf (documentation 'with-locked-queue 'function)
"Execute `body' with the queue lock held. Use the `/no-lock' functions
inside `body'.")

(do-external-symbols (sym *package*)
  (let ((name (string-downcase (string sym))))
    (when (search "/no-lock" name)
      (setf (documentation sym 'function)
            (format nil
                    "Like `~a' but does not acquire the lock. ~
                     Use inside~%`with-locked-queue'."
                    (subseq name 0 (position #\/ name :from-end t)))))))
