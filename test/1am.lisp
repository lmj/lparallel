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

;;; 1am implements a minimal subset of the 5am/eos testing API. Some
;;; reasons for its existence:
;;;
;;; * Compiling tests is ~8x faster than 5am/eos configured with
;;; :compile-at :definition-time.
;;;
;;; * Compiling tests does not cause the default heap size to be
;;; exceeded on some platforms (32-bit SBCL, Allegro Express).
;;;
;;; * Checks may occur inside threads.
;;;
;;; * Test order is randomized on each run.
;;;
;;; * Type inferencing works inside the `is' macro.
;;;
;;; Although some or all of these issues may be addressed by 5am or
;;; eos in the future, until then 1am serves an immediate need.

(defpackage #:lparallel-test.1am
  (:use #:cl)
  (:export #:is #:signals #:test #:run! #:debug! #:in-suite*))

(in-package #:lparallel-test.1am)

(defvar *tests* nil)
(defvar *pass-count* 0)
(defvar *running* nil)

(defun passed ()
  (incf *pass-count*)
  (format t "."))

(defmacro is (form)
  `(progn
     (assert ,form)
     (passed)))

(defun check-signals (expected fn)
  (flet ((handler (condition)
           (cond ((typep condition expected)
                  (passed)
                  (return-from check-signals))
                 (t
                  (error "Expected to signal ~s, but got ~a."
                         expected (if condition
                                      (type-of condition)
                                      "nothing"))))))
    (handler-bind ((condition #'handler))
      (funcall fn))
    (handler nil)))

(defmacro signals (condition &body body)
  `(check-signals ',condition (lambda () ,@body)))

(defun report ()
  (format t "~& Did ~d check~:p.~%    ~
                   Pass: ~d (100%)~%    ~
                   Skip: 0 ( 0%)~%    ~
                   Fail: 0 ( 0%)~%"
          *pass-count* *pass-count*))

(defmacro test (name &body body)
  `(progn
     (defun ,name ()
       (unless *running* (setf *pass-count* 0))
       (multiple-value-prog1 (progn ,@body)
         (unless *running* (report))))
     (pushnew ',name *tests*)
     ',name))

(defun shuffle (vector)
  (let ((*random-state* (make-random-state t)))
    (loop
       :for i :downfrom (- (length vector) 1) :to 1
       :do (rotatef (aref vector i) (aref vector (random (1+ i))))))
  vector)

(defun make-test-seq (test-spec)
  (etypecase test-spec
    (atom (vector test-spec))
    (sequence (shuffle (map-into (make-array (length test-spec))
                                 #'identity test-spec)))))

(defun run! (&optional (test-spec *tests*))
  (setf *pass-count* 0)
  (let ((*running* t))
    (map nil #'funcall (make-test-seq test-spec))
    (report)))

(defun debug! (&rest args)
  (apply #'run! args))

(defun in-suite* (&rest args)
  (declare (ignore args)))
