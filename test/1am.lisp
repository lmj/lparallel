;;; Copyright (c) 2014, James M. Lawrence. All rights reserved.
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

;;; 1am implements a minimal variant of the 5am/eos testing API. Some
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
;;; * Tests are runnable as ordinary functions of the same name.

(defpackage #:lparallel-test.1am
  (:use #:cl)
  (:export #:is #:signals #:test #:run))

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
  (flet ((failed (got)
           (error "Expected to signal ~s, but got ~a." expected got)))
    (handler-case (progn
                    (funcall fn)
                    (failed "nothing"))
      (condition (condition)
        (if (typep condition expected)
            (passed)
            (failed (type-of condition)))))))

(defmacro signals (condition &body body)
  `(check-signals ',condition (lambda () ,@body)))

(defun report (tests)
  (format t "~&~%Success: ~s test~:p, ~s check~:p.~%"
          (length tests) *pass-count*)
  (values))

(defun shuffle (vector)
  (loop
     :for i :downfrom (- (length vector) 1) :to 1
     :do (rotatef (aref vector i) (aref vector (random (1+ i)))))
  vector)

(defun run (&optional (tests *tests*))
  (setf *pass-count* 0)
  (let ((*running* t))
    (map nil #'funcall (let ((*random-state* (make-random-state t)))
                         (shuffle (map 'vector #'identity tests)))))
  (report tests))

(defun call-standalone-test (name fn)
  (setf *pass-count* 0)
  (multiple-value-prog1 (funcall fn)
    (report (list name))))

(defun call-test (name fn)
  (format t "~&~a~%" name)
  (finish-output)
  (if *running*
      (funcall fn)
      (call-standalone-test name fn)))

(defmacro test (name &body body)
  `(progn
     (defun ,name ()
       (call-test ',name (lambda () ,@body)))
     (pushnew ',name *tests*)
     ',name))
