;;; Copyright (c) 2011, James M. Lawrence. All rights reserved.
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

(lp-test queue-test
  (dolist (n (loop :for i :below 20 :collect i))
    (let1 q (make-queue n)
      (is (eq t (queue-empty-p q)))
      (multiple-value-bind (a b) (try-pop-queue q)
        (is (null a))
        (is (null b)))
      (multiple-value-bind (a b) (peek-queue q)
        (is (null a))
        (is (null b)))
      (push-queue 3 q)
      (is (eq nil (queue-empty-p q)))
      (push-queue 4 q)
      (is (eq nil (queue-empty-p q)))
      (multiple-value-bind (a b) (peek-queue q)
        (is (= 3 a))
        (is (not (null b))))
      (push-queue 5 q)
      (push-queue 6 q)
      (push-queue 7 q)
      (is (eql 5 (queue-count q)))
      (is (eql 3 (pop-queue q)))
      (multiple-value-bind (a b) (try-pop-queue q)
        (is (= 4 a))
        (is (not (null b))))
      (is (equal '(5 6 7)
                 (loop :repeat 3 :collect (pop-queue q))))
      (is (eq t (queue-empty-p q)))
      (multiple-value-bind (a b) (try-pop-queue q)
        (is (null a))
        (is (null b)))
      (multiple-value-bind (a b) (peek-queue q)
        (is (null a))
        (is (null b)))
      (push-queue 88 q)
      (is (eq nil (queue-empty-p q)))
      (is (eq 1 (queue-count q)))
      (pop-queue q)
      (is (eq t (queue-empty-p q)))
      (is (eq 0 (queue-count q))))))
