;;; Copyright (c) 2017, James M. Lawrence. All rights reserved.
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

(define-dynamic-task add-foo-bar (x)
  (declare (special *foo* *bar*))
  (+ x *foo* *bar*))

(full-test dynamic-task-test
  (with-dynamic-tasks ((*foo* 3) (*bar* 100))
    (declare (special *foo* *bar*))
    (is (equal '(104 105 106)
               (pmapcar #'add-foo-bar '(1 2 3)))))

  (let ((*dynamic-task-vars* '(*foo*)))
    (let ((*foo* 3))
      (declare (special *foo*))
      (with-dynamic-tasks ((*bar* 100))
        (declare (special *bar*))
        (is (equal '(104 105 106)
                   (pmapcar #'add-foo-bar '(1 2 3)))))))

  (let ((*dynamic-task-vars* '(*foo* *bar*))
        (task (dynamic-task (x)
                (declare (special *foo* *bar*))
                (+ x *foo* *bar*))))
    (let ((*foo* 3))
      (declare (special *foo*))
      (with-dynamic-tasks ((*bar* 100))
        (declare (special *bar*))
        (is (equal '(104 105 106)
                   (pmapcar task '(1 2 3)))))))

  (let ((*dynamic-task-vars* '(*memo*)))
    (with-dynamic-tasks ((*memo* 100))
      (is (equal '(101 102 103)
                 (pmapcar (dynamic-task (x)
                            (+ x *memo*))
                          '(1 2 3)))))
    (let ((*memo* 10))
      (with-dynamic-tasks ()
        (is (equal '(11 12 13)
                   (pmapcar (dynamic-task (x)
                              (+ x *memo*))
                            '(1 2 3)))))))

  (let ((*dynamic-task-vars* nil))
    (with-dynamic-tasks ((*baz* 100))
      (declare (special *baz*))
      (is (equal '(101 102 103)
                 (pmapcar (dynamic-task (x)
                            (declare (special *baz*))
                            (+ x *baz*))
                          '(1 2 3))))))

  (let ((*dynamic-task-vars* nil))
    (with-dynamic-tasks ()
      (is (equal '(2 3 4)
                 (pmapcar #'1+ '(1 2 3)))))))
