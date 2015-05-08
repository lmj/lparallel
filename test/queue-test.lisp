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
  `(base-test ,name
     (let ((q (,make-queue)))
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
                  (loop repeat 3 collect (,pop-queue q))))
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
       (is (eq 0 (,queue-count q))))))

(defun make-fixed-capacity-queue ()
  (make-queue :fixed-capacity 20))

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

#+lparallel.with-stealing-scheduler
(define-queue-test spin-queue-test
  :make-queue    lparallel.spin-queue:make-spin-queue
  :push-queue    lparallel.spin-queue:push-spin-queue
  :pop-queue     lparallel.spin-queue:pop-spin-queue
  :try-pop-queue lparallel.spin-queue:pop-spin-queue
  :queue-empty-p lparallel.spin-queue:spin-queue-empty-p
  :queue-count   lparallel.spin-queue:spin-queue-count
  :peek-queue    lparallel.spin-queue:peek-spin-queue)

(define-queue-test fixed-capacity-queue-test
  :make-queue    make-fixed-capacity-queue
  :push-queue    push-queue
  :pop-queue     pop-queue
  :try-pop-queue try-pop-queue
  :queue-empty-p queue-empty-p
  :queue-count   queue-count
  :peek-queue    peek-queue)

#-lparallel.with-stealing-scheduler
(define-queue-test biased-queue-test
  :make-queue    lparallel.biased-queue:make-biased-queue
  :push-queue    lparallel.biased-queue:push-biased-queue
  :pop-queue     lparallel.biased-queue:pop-biased-queue
  :try-pop-queue lparallel.biased-queue:try-pop-biased-queue
  :queue-empty-p lparallel.biased-queue:biased-queue-empty-p
  :queue-count   lparallel.biased-queue:biased-queue-count
  :peek-queue    lparallel.biased-queue:peek-biased-queue)

#-lparallel.with-stealing-scheduler
(define-queue-test biased-queue-low-test
  :make-queue    lparallel.biased-queue:make-biased-queue
  :push-queue    lparallel.biased-queue:push-biased-queue/low
  :pop-queue     lparallel.biased-queue:pop-biased-queue
  :try-pop-queue lparallel.biased-queue:try-pop-biased-queue
  :queue-empty-p lparallel.biased-queue:biased-queue-empty-p
  :queue-count   lparallel.biased-queue:biased-queue-count
  :peek-queue    lparallel.biased-queue:peek-biased-queue)

(defmacro define-grind-queue (name
                              &key make-queue push-queue pop-queue queue-count)
  `(base-test ,name
     (flet ((grind (producer-count consumer-count objects-per-producer)
              (let ((data-queue (,make-queue))
                    (done-queue (,make-queue)))
                (repeat producer-count
                  (with-thread ()
                    (repeat objects-per-producer
                      (,push-queue 'hello data-queue))
                    (,push-queue t done-queue)))
                (repeat consumer-count
                  (with-thread ()
                    (loop until (eq :end (,pop-queue data-queue)))
                    (,push-queue t done-queue)))
                (repeat producer-count
                  (,pop-queue done-queue))
                (repeat consumer-count
                  (,push-queue :end data-queue))
                (repeat consumer-count
                  (,pop-queue done-queue))
                (is (= 0 (,queue-count data-queue))))))
       (with-thread-count-check
         (let ((n 100000))
           (grind 4 1 n)
           (grind 1 4 n)
           (grind 1 1 n)
           (grind 4 4 n))
         (sleep 0.5)))))

(defun try-pop-queue/wait/no-timeout (queue)
  (loop (multiple-value-bind (value presentp) (try-pop-queue queue)
          (when presentp
            (return value)))))

#-lparallel.without-bordeaux-threads-condition-wait-timeout
(defun try-pop-queue/wait/timeout (queue)
  (multiple-value-bind (value presentp) (try-pop-queue queue :timeout 9999)
    (assert presentp)
    value))

(define-grind-queue grind-queue-test
    :make-queue  make-queue
    :push-queue  push-queue
    :pop-queue   pop-queue
    :queue-count queue-count)

#-lparallel.with-green-threads
(define-grind-queue grind-queue-no-timeout-test
    :make-queue  make-queue
    :push-queue  push-queue
    :pop-queue   try-pop-queue/wait/no-timeout
    :queue-count queue-count)

#-lparallel.without-bordeaux-threads-condition-wait-timeout
(define-grind-queue grind-queue-timeout-test
    :make-queue  make-queue
    :push-queue  push-queue
    :pop-queue   try-pop-queue/wait/timeout
    :queue-count queue-count)

(define-grind-queue grind-fixed-capacity-queue-test
    :make-queue  make-fixed-capacity-queue
    :push-queue  push-queue
    :pop-queue   pop-queue
    :queue-count queue-count)

#-lparallel.with-green-threads
(define-grind-queue grind-fixed-capacity-queue-no-timeout-test
    :make-queue  make-fixed-capacity-queue
    :push-queue  push-queue
    :pop-queue   try-pop-queue/wait/no-timeout
    :queue-count queue-count)

#-lparallel.without-bordeaux-threads-condition-wait-timeout
(define-grind-queue grind-fixed-capacity-queue-timeout-test
    :make-queue  make-fixed-capacity-queue
    :push-queue  push-queue
    :pop-queue   try-pop-queue/wait/timeout
    :queue-count queue-count)

#+lparallel.with-stealing-scheduler
(defun pop-spin-queue/wait (queue)
  (loop (multiple-value-bind
              (item presentp) (lparallel.spin-queue:pop-spin-queue queue)
          (when presentp
            (return item)))))

#+lparallel.with-stealing-scheduler
(define-grind-queue grind-spin-queue-test
    :make-queue  lparallel.spin-queue:make-spin-queue
    :push-queue  lparallel.spin-queue:push-spin-queue
    :pop-queue   pop-spin-queue/wait
    :queue-count lparallel.spin-queue:spin-queue-count)

#-lparallel.with-stealing-scheduler
(define-grind-queue grind-biased-queue-test
    :make-queue  lparallel.biased-queue:make-biased-queue
    :push-queue  lparallel.biased-queue:push-biased-queue
    :pop-queue   lparallel.biased-queue:pop-biased-queue
    :queue-count lparallel.biased-queue:biased-queue-count)

#-lparallel.with-stealing-scheduler
(define-grind-queue grind-biased-queue-low-test
    :make-queue  lparallel.biased-queue:make-biased-queue
    :push-queue  lparallel.biased-queue:push-biased-queue/low
    :pop-queue   lparallel.biased-queue:pop-biased-queue
    :queue-count lparallel.biased-queue:biased-queue-count)

(base-test filled-queue-test
  (loop for n from 1 to 3
        do (let ((queue (make-queue :fixed-capacity n)))
             (repeat n
               (is (not (queue-full-p queue)))
               (push-queue :x queue))
             (repeat 3
               (is (queue-full-p queue))
               (let ((pushedp nil))
                 (with-thread ()
                   (push-queue :x queue)
                   (setf pushedp t))
                 (sleep 0.2)
                 (is (null pushedp))
                 (is (eq :x (pop-queue queue)))
                 (sleep 0.2)
                 (is (eq t pushedp))))))
  (loop for n from 1 to 3
        do (let ((queue (make-queue :fixed-capacity n)))
             (repeat n
               (is (not (queue-full-p queue)))
               (push-queue :x queue))
             (repeat 3
               (is (queue-full-p queue))
               (let ((pushedp nil))
                 (with-thread ()
                   (push-queue :x queue)
                   (setf pushedp t))
                 (sleep 0.2)
                 (is (null pushedp))
                 (is (equal '(:x t)
                            (multiple-value-list (try-pop-queue queue))))
                 (sleep 0.2)
                 (is (eq t pushedp)))))))

(base-test queue-initial-contents-test
  (let ((q (make-queue :initial-contents '(3 4 5))))
    (is (= 3 (pop-queue q)))
    (is (= 4 (pop-queue q)))
    (is (= 5 (pop-queue q)))
    (is (queue-empty-p q)))
  (let ((q (make-queue :initial-contents #(3 4 5))))
    (is (= 3 (pop-queue q)))
    (is (= 4 (pop-queue q)))
    (is (= 5 (pop-queue q)))
    (is (queue-empty-p q)))
  (let ((q (make-queue :fixed-capacity 10 :initial-contents '(3 4 5))))
    (is (= 3 (pop-queue q)))
    (is (= 4 (pop-queue q)))
    (is (= 5 (pop-queue q)))
    (is (queue-empty-p q)))
  (let ((q (make-queue :fixed-capacity 3 :initial-contents #(3 4 5))))
    (is (queue-full-p q))
    (is (= 3 (pop-queue q)))
    (is (= 4 (pop-queue q)))
    (is (= 5 (pop-queue q)))
    (is (queue-empty-p q)))
  (let ((q (make-queue :fixed-capacity 2 :initial-contents #(3 4 5))))
    (is (queue-full-p q))
    (is (= 3 (pop-queue q)))
    (is (= 4 (pop-queue q)))
    (is (queue-empty-p q))))

#-lparallel.without-bordeaux-threads-condition-wait-timeout
(base-test queue-timeout-test
  (dolist (q (list (make-queue) (make-queue :fixed-capacity 10)))
    (multiple-value-bind (a b) (try-pop-queue q :timeout nil)
      (is (null a))
      (is (null b)))
    (multiple-value-bind (a b) (try-pop-queue q :timeout 0)
      (is (null a))
      (is (null b)))
    (multiple-value-bind (a b) (try-pop-queue q :timeout -999999)
      (is (null a))
      (is (null b)))
    (multiple-value-bind (a b) (try-pop-queue q :timeout 0.2)
      (is (null a))
      (is (null b)))
    (signals type-error
      (try-pop-queue q :timeout "foo"))
    (let ((flag nil))
      (with-thread ()
        (sleep 0.4)
        (setf flag t))
      (multiple-value-bind (a b) (try-pop-queue q :timeout 0.2)
        (is (null a))
        (is (null b))
        (is (queue-empty-p q))
        (is (null flag))
        (sleep 0.4)
        (is (eq t flag))))
    (with-thread ()
      (sleep 0.2)
      (push-queue 99 q))
    (multiple-value-bind (a b) (try-pop-queue q :timeout 0.4)
      (is (= 99 a))
      (is (identity b))
      (is (queue-empty-p q)))
    (push-queue 3 q)
    (multiple-value-bind (a b) (try-pop-queue q :timeout -99999)
      (is (= 3 a))
      (is (identity b)))
    (push-queue 4 q)
    (multiple-value-bind (a b) (try-pop-queue q :timeout 0)
      (is (= 4 a))
      (is (identity b)))
    (push-queue 5 q)
    (multiple-value-bind (a b) (try-pop-queue q :timeout 0.2)
      (is (= 5 a))
      (is (identity b)))
    (is (queue-empty-p q))))

#-lparallel.without-bordeaux-threads-condition-wait-timeout
(base-test queue-small-timeout-test
  (dolist (q (list (make-queue) (make-queue :fixed-capacity 1)))
    (dolist (use-float-p '(nil t))
      (loop for e from -3 downto -20
            for timeout = (let ((value (expt 10 e)))
                            (if use-float-p
                                (float value)
                                value))
            do (repeat 10
                 (multiple-value-bind (a b) (try-pop-queue q :timeout timeout)
                   (is (null a))
                   (is (null b))))))))
