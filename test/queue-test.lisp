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

(defmacro define-queue-test (name
                             &key
                             make-queue
                             push-queue
                             pop-queue
                             try-pop-queue
                             queue-empty-p
                             queue-count
                             peek-queue)
  `(lp-base-test ,name
     (dolist (n (loop :for i :below 20 :collect i))
       (let ((q (,make-queue n)))
         (is (eq t (,queue-empty-p q)))
         (multiple-value-bind (a b) (,try-pop-queue q)
           (is (null a))
           (is (null b)))
         (multiple-value-bind (a b) (,peek-queue q)
           (is (null a))
           (is (null b)))
         (,push-queue 3 q)
         (is (eq nil (,queue-empty-p q)))
         (,push-queue 4 q)
         (is (eq nil (,queue-empty-p q)))
         (multiple-value-bind (a b) (,peek-queue q)
           (is (= 3 a))
           (is (not (null b))))
         (,push-queue 5 q)
         (,push-queue 6 q)
         (,push-queue 7 q)
         (is (eql 5 (,queue-count q)))
         (is (eql 3 (,pop-queue q)))
         (multiple-value-bind (a b) (,try-pop-queue q)
           (is (= 4 a))
           (is (not (null b))))
         (is (equal '(5 6 7)
                    (loop :repeat 3 :collect (,pop-queue q))))
         (is (eq t (,queue-empty-p q)))
         (multiple-value-bind (a b) (,try-pop-queue q)
           (is (null a))
           (is (null b)))
         (multiple-value-bind (a b) (,peek-queue q)
           (is (null a))
           (is (null b)))
         (,push-queue 88 q)
         (is (eq nil (,queue-empty-p q)))
         (is (eq 1 (,queue-count q)))
         (,pop-queue q)
         (is (eq t (,queue-empty-p q)))
         (is (eq 0 (,queue-count q)))))))

(define-queue-test raw-queue-test
  :make-queue    make-raw-queue
  :push-queue    push-raw-queue
  :pop-queue     pop-raw-queue
  :try-pop-queue pop-raw-queue
  :queue-empty-p raw-queue-empty-p
  :queue-count   raw-queue-count
  :peek-queue    peek-raw-queue)

(define-queue-test queue-test
  :make-queue    make-queue
  :push-queue    push-queue
  :pop-queue     pop-queue
  :try-pop-queue try-pop-queue
  :queue-empty-p queue-empty-p
  :queue-count   queue-count
  :peek-queue    peek-queue)

(define-queue-test spin-queue-test
  :make-queue    make-spin-queue
  :push-queue    push-spin-queue
  :pop-queue     pop-spin-queue
  :try-pop-queue pop-spin-queue
  :queue-empty-p spin-queue-empty-p
  :queue-count   spin-queue-count
  :peek-queue    peek-spin-queue)

(defmacro define-grind-queue (name
                              &key make-queue push-queue pop-queue queue-count)
  `(lp-base-test ,name
     (let ((obj-count 100000)
           (iter-count 2))
       (with-thread-count-check
         (dolist (thread-count '(1 2 3 4 8 16 32 64 128))
           (let ((to-workers (,make-queue))
                 (from-workers (,make-queue)))
             (with-thread (:name "grind-queue"
                           :bindings `((*standard-output* .
                                        ,*standard-output*)))
               (loop (let ((obj (,pop-queue to-workers)))
                       (if obj
                           (,push-queue obj from-workers)
                           (return)))))
             (repeat iter-count
               (dotimes (i obj-count)
                 (,push-queue 'hello to-workers))
               (dotimes (i obj-count)
                 (,pop-queue from-workers))
               (is (zerop (,queue-count to-workers)))
               (is (zerop (,queue-count from-workers))))
             (repeat thread-count
               (,push-queue nil to-workers))))
         (sleep 0.5)))))

(define-grind-queue grind-queue-test
    :make-queue  make-queue
    :push-queue  push-queue
    :pop-queue   pop-queue
    :queue-count queue-count)

(defun pop-spin-queue/wait (queue)
  (loop (multiple-value-bind (item presentp) (pop-spin-queue queue)
          (when presentp
            (return item)))))

(define-grind-queue grind-spin-queue-test
    :make-queue  make-spin-queue
    :push-queue  push-spin-queue
    :pop-queue   pop-spin-queue/wait
    :queue-count spin-queue-count)

(define-grind-queue grind-biased-queue-1-test
    :make-queue  lparallel.biased-queue:make-biased-queue
    :push-queue  lparallel.biased-queue:push-biased-queue
    :pop-queue   lparallel.biased-queue:pop-biased-queue
    :queue-count lparallel.biased-queue:biased-queue-count)

(define-grind-queue grind-biased-queue-2-test
    :make-queue  lparallel.biased-queue:make-biased-queue
    :push-queue  lparallel.biased-queue:push-biased-queue/low
    :pop-queue   lparallel.biased-queue:pop-biased-queue
    :queue-count lparallel.biased-queue:biased-queue-count)
