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

(lp-test futures-test
  (let ((a (future 3))
        (b (future 4)))
    (is (= 7 (+ (force a) (force b)))))
  (let1 a (future 5)
    (sleep 0.1)
    (is (fulfilledp a))
    (is (= 5 (force a))))
  (let1 a (future (sleep 0.2) 3)
    (is (not (fulfilledp a)))
    (sleep 0.1)
    (is (eq nil (fulfill a 4)))
    (is (not (fulfilledp a)))
    (is (= 3 (force a))))
  (let1 a (future 3)
    (sleep 0.1)
    (is (eq nil (fulfill a 9)))
    (is (= 3 (force a)))))

(lp-test promises-test
  (let ((a (promise))
        (b (promise)))
    (fulfill a 3)
    (fulfill b 4)
    (is (= 12 (* (force a) (force b)))))
  (let1 a (promise)
    (is (eq t (fulfill a 3)))
    (is (eq nil (fulfill a 4)))
    (is (= 3 (force a)))))

(lp-test fulfill-chain-test
  (let ((a (promise))
        (b (promise)))
    (fulfill a (chain (future 5)))
    (fulfill b (chain (future 6)))
    (is (= 30 (* (force a) (force b)))))
  (let ((a (promise))
        (b (promise)))
    (fulfill a (chain (delay 7)))
    (fulfill b (chain (delay 8)))
    (is (= 56 (* (force a) (force b))))))

(lp-test force-chain-test
  (let1 f (delay (chain (delay 3)))
    (is (= 3 (force f))))
  (let1 f (delay (chain (delay (chain (delay 3)))))
    (is (= 3 (force f)))))

(lp-base-test speculations-test
  (setf *memo* (make-queue))
  (with-new-kernel (2)
    (future (sleep 0.25))
    (future (sleep 0.50))
    (sleep 0.125)
    (speculate (push-queue 3 *memo*))
    (future (push-queue 4 *memo*))
    (sleep 0.5)
    (is (eql 4 (pop-queue *memo*)))
    (is (eql 3 (pop-queue *memo*)))))

(lp-test flood-test
  (let* ((a (promise))
         (futures (collect-n 20 (future (force a)))))
    (is (every (complement #'fulfilledp) futures))
    (sleep 0.1)
    (is (every (complement #'fulfilledp) futures))
    (fulfill a 4)
    (sleep 0.1)
    (is (every #'fulfilledp futures))
    (is (every (lambda (x) (= x 4)) (mapcar #'force futures)))))

(defmacro define-force-test (defer)
  `(lp-test ,(intern (concatenate
                      'string (string defer) (string '#:-force-test)))
     (let1 a (,defer (+ 3 4))
       (is (= 7 (force a))))
     (setf *memo* 0)
     (let1 a (,defer (progn (incf *memo*) 9))
       (sleep 0.1)
       (is (= 9 (force a)))
       (is (= 1 *memo*))
       (is (= 9 (force a)))
       (is (= 1 *memo*)))
     (let1 a (mapcar (lambda (x) (,defer (* x x))) '(3 4 5))
       (is (equal '(9 16 25) (mapcar #'force a))))
     (kernel-handler-bind ((foo-error (lambda (e)
                                        (invoke-restart 'transfer-error e))))
       (let1 a (,defer (error 'foo-error))
         (signals foo-error
           (force a))
         (signals foo-error
           (force a))))))

(define-force-test future)
(define-force-test speculate)
(define-force-test delay)

(lp-test sequential-force-test
  (repeat 100
    (let* ((a (future 3))
           (b (future (force a)))
           (c (future (force b)))
           (d (future (force c)))
           (e (future (force d))))
      (is (= 3 (force e))))))

(defmacro define-big-sequential ()
  `(lp-test big-sequential-test
     ,(loop
         :with vars := (collect-n 100 (gensym))
         :for (a b) :on vars
         :when b :collect `(,b (future (force ,a))) :into binds
         :finally (return `(let* ((,(car vars) (future 4)) ,@binds)
                             (is (= 4 (force ,(car (last vars))))))))))

(define-big-sequential)

(lp-test future-recursion-test
  (labels ((fib (n)
             (if (< n 2)
                 n
                 (let* ((f1 (future (fib (- n 1))))
                        (f2 (fib (- n 2))))
                   (+ (force f1) f2)))))
    (is (= 144 (fib 12)))))

(lp-test multiple-value-test
  (let1 x (promise)
    (fulfill x (values 3 4 5))
    (multiple-value-bind (p q r) (force x)
      (is (= 3 p))
      (is (= 4 q))
      (is (= 5 r))))
  (let1 x (future (values 3 4 5))
    (multiple-value-bind (p q r) (force x)
      (is (= 3 p))
      (is (not (null q)))
      (is (= 4 q))
      (is (= 5 r))))
  (let1 x (delay (values 3 4 5))
    (multiple-value-bind (p q r) (force x)
      (is (= 3 p))
      (is (not (null q)))
      (is (= 4 q))
      (is (= 5 r)))))

(lp-test future-restart-test
  (kernel-handler-bind ((foo-error (lambda (e)
                                     (declare (ignore e))
                                     (invoke-restart 'eleven))))
    (let ((x (future (restart-case (error 'foo-error)
                       (eleven () 11)))))
      (is (eql 11 (force x)))))
  (kernel-handler-bind ((foo-error (lambda (e)
                                     (declare (ignore e))
                                     (invoke-restart 'eleven))))
    (let* ((x (future (restart-case (error 'foo-error)
                        (eleven () 11))))
           (y (future (force x))))
      (is (eql 11 (force y))))))

(lp-test speculation-restart-test
  (kernel-handler-bind ((foo-error (lambda (e)
                                     (declare (ignore e))
                                     (invoke-restart 'eleven))))
    (let ((x (speculate (restart-case (error 'foo-error)
                          (eleven () 11)))))
      (is (eql 11 (force x)))))
  (kernel-handler-bind ((foo-error (lambda (e)
                                     (declare (ignore e))
                                     (invoke-restart 'eleven))))
    (let* ((x (speculate (restart-case (error 'foo-error)
                           (eleven () 11))))
           (y (force x)))
      (is (eql 11 (force y))))))

(lp-test fulfill-delay-restart-test
  (kernel-handler-bind ((error (lambda (e)
                                 (invoke-restart 'transfer-error e))))
    (handler-bind ((foo-error (lambda (e)
                                (declare (ignore e))
                                (invoke-restart 'store-value 3 4))))
      (let ((x (future (error 'foo-error))))
        (is (equal '(3 4) (multiple-value-list (force x))))))))

(lp-test fulfill-future-restart-test
  (kernel-handler-bind ((error (lambda (e)
                                 (invoke-restart 'transfer-error e))))
    (handler-bind ((foo-error (lambda (e)
                                (declare (ignore e))
                                (invoke-restart 'store-value 3 4))))
      (let ((x (future (error 'foo-error))))
        (is (equal '(3 4) (multiple-value-list (force x))))))))

(lp-base-test kill-future-test
  (handler-bind ((warning (lambda (w)
                            (when-let (r (find-restart 'muffle-warning w))
                              (invoke-restart r)))))
    (with-new-kernel (2)
      (let1 main-thread (current-thread)
        (kernel-handler-bind
            ((foo-error (lambda (e)
                          (declare (ignore e))
                          ;; don't kill main thread
                          (unless (eq (current-thread) main-thread)
                            (invoke-abort-thread)))))
          (let1 future (future
                         (unless (eq (current-thread) main-thread)
                           (setf *error-output* (make-broadcast-stream)))
                         (error 'foo-error))
            (sleep 0.1)
            (signals task-killed-error
              (force future))))))))
