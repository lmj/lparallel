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

(full-test futures-test
  (let ((a (future 3))
        (b (future 4)))
    (is (= 7 (+ (force a) (force b)))))
  (let ((a (future 5)))
    (sleep 0.2)
    (is (fulfilledp a))
    (is (= 5 (force a))))
  (let ((a (future (sleep 0.4) 3)))
    (is (not (fulfilledp a)))
    (sleep 0.2)
    (is (eq nil (fulfill a 4)))
    (is (not (fulfilledp a)))
    (is (= 3 (force a))))
  (let ((a (future 3)))
    (sleep 0.2)
    (is (eq nil (fulfill a 9)))
    (is (= 3 (force a)))))

(full-test promises-test
  (let ((a (promise))
        (b (promise)))
    (fulfill a 3)
    (fulfill b 4)
    (is (= 12 (* (force a) (force b)))))
  (let ((a (promise)))
    (is (eq t (fulfill a 3)))
    (is (eq nil (fulfill a 4)))
    (is (= 3 (force a)))))

(full-test fulfill-chain-test
  (let ((a (promise))
        (b (promise)))
    (fulfill a (chain (future 5)))
    (fulfill b (chain (future 6)))
    (is (= 30 (* (force a) (force b)))))
  (let ((a (promise))
        (b (promise)))
    (fulfill a (chain (delay 7)))
    (fulfill b (chain (delay 8)))
    (is (= 56 (* (force a) (force b)))))
  (let* ((a (promise))
         (b (chain a)))
    (fulfill b 3)
    (is (eql 3 (force b)))
    (is (eql 3 (force a)))))

(full-test force-chain-test
  (let ((f (delay (chain (delay 3)))))
    (is (= 3 (force f))))
  (let ((f (delay (chain (delay (chain (delay 3)))))))
    (is (= 3 (force f)))))

(full-test nested-chain-test
  (is (equal '(3 4 5)
             (multiple-value-list
              (force (chain (speculate (chain (speculate (values 3 4 5)))))))))
  (is (equal '(3 4 5)
             (multiple-value-list
              (force (chain (future (chain (future (values 3 4 5)))))))))
  (is (equal '(3 4 5)
             (multiple-value-list
              (force (chain (delay (chain (delay (values 3 4 5)))))))))
  (let ((f (future (values 3 4 5))))
    (sleep 0.1)
    (is (equal '(3 4 5)
               (multiple-value-list
                (force (chain (delay (chain f)))))))
    (is (equal '(3 4 5)
               (multiple-value-list
                (force (chain (future (chain f)))))))))

(base-test speculations-test
  (setf *memo* (make-queue))
  (with-temp-kernel (2)
    (sleep 0.2)
    (future (sleep 0.25))
    (future (sleep 0.50))
    (sleep 0.125)
    (speculate (push-queue 3 *memo*))
    (future (push-queue 4 *memo*))
    (sleep 0.5)
    (is (eql 4 (pop-queue *memo*)))
    (is (eql 3 (pop-queue *memo*)))))

(full-test flood-test
  (let* ((a (promise))
         (futures (collect-n 100 (future (force a)))))
    (is (notany #'fulfilledp futures))
    (sleep 0.5)
    (is (notany #'fulfilledp futures))
    (fulfill a 4)
    (sleep 1.0)
    (is (every #'fulfilledp futures))
    (is (every (lambda (x) (= x 4)) (mapcar #'force futures)))))

(defmacro define-force-test (defer)
  `(full-test ,(intern (concatenate
                      'string (string defer) (string '#:-force-test)))
     (let ((a (,defer (+ 3 4))))
       (is (= 7 (force a))))
     (setf *memo* 0)
     (let ((a (,defer (progn (incf *memo*) 9))))
       (sleep 0.1)
       (is (= 9 (force a)))
       (is (= 1 *memo*))
       (is (= 9 (force a)))
       (is (= 1 *memo*)))
     (let ((a (mapcar (lambda (x) (,defer (* x x))) '(3 4 5))))
       (is (equal '(9 16 25) (mapcar #'force a))))
     (task-handler-bind ((foo-error (lambda (e)
                                      (invoke-restart 'transfer-error e))))
       (let ((a (,defer (error 'foo-error))))
         (signals foo-error
           (force a))
         (signals foo-error
           (force a))))))

(define-force-test future)
(define-force-test speculate)
(define-force-test delay)

(full-test sequential-force-test
  (repeat 100
    (let* ((a (future 3))
           (b (future (force a)))
           (c (future (force b)))
           (d (future (force c)))
           (e (future (force d))))
      (is (= 3 (force e))))))

(defmacro define-big-sequential ()
  `(full-test big-sequential-test
     ,(loop with vars = (collect-n 100 (gensym))
            for (a b) on vars
            when b collect `(,b (future (force ,a))) into binds
            finally (return `(let* ((,(car vars) (future 4)) ,@binds)
                               (is (= 4 (force ,(car (last vars))))))))))

(define-big-sequential)

(full-test future-recursion-test
  (labels ((fib (n)
             (if (< n 2)
                 n
                 (let* ((f1 (future (fib (- n 1))))
                        (f2 (fib (- n 2))))
                   (+ (force f1) f2)))))
    (is (= 144 (fib 12)))))

(full-test multiple-value-test
  (let ((x (promise)))
    (fulfill x (values 3 4 5))
    (multiple-value-bind (p q r) (force x)
      (is (= 3 p))
      (is (= 4 q))
      (is (= 5 r))))
  (let ((x (future (values 3 4 5))))
    (multiple-value-bind (p q r) (force x)
      (is (= 3 p))
      (is (not (null q)))
      (is (= 4 q))
      (is (= 5 r))))
  (let ((x (delay (values 3 4 5))))
    (multiple-value-bind (p q r) (force x)
      (is (= 3 p))
      (is (not (null q)))
      (is (= 4 q))
      (is (= 5 r)))))

(full-test future-restart-test
  (task-handler-bind ((foo-error (lambda (e)
                                   (declare (ignore e))
                                   (invoke-restart 'eleven))))
    (let ((x (future (restart-case (error 'foo-error)
                       (eleven () 11)))))
      (is (eql 11 (force x)))))
  (task-handler-bind ((foo-error (lambda (e)
                                   (declare (ignore e))
                                   (invoke-restart 'eleven))))
    (let* ((x (future (restart-case (error 'foo-error)
                        (eleven () 11))))
           (y (future (force x))))
      (is (eql 11 (force y))))))

(full-test speculation-restart-test
  (task-handler-bind ((foo-error (lambda (e)
                                   (declare (ignore e))
                                   (invoke-restart 'eleven))))
    (let ((x (speculate (restart-case (error 'foo-error)
                          (eleven () 11)))))
      (is (eql 11 (force x)))))
  (task-handler-bind ((foo-error (lambda (e)
                                   (declare (ignore e))
                                   (invoke-restart 'eleven))))
    (let* ((x (speculate (restart-case (error 'foo-error)
                           (eleven () 11))))
           (y (force x)))
      (is (eql 11 (force y))))))

(full-test future-store-value-test
  (task-handler-bind ((error (lambda (e)
                               (invoke-restart 'transfer-error e))))
    (handler-bind ((foo-error (lambda (e)
                                (declare (ignore e))
                                (invoke-restart 'store-value 3 4))))
      (let ((x (future (error 'foo-error))))
        (sleep 0.1)
        (is (equal '(3 4) (multiple-value-list (force x))))))))

(base-test multi-future-store-value-test
  ;; verify STORE-VALUE is thread-safe
  (loop for n from 1 to 64
        do (with-temp-kernel (n)
              (let* ((channel (make-channel))
                     (counter (make-queue))
                     (future  (task-handler-bind
                                  ((foo-error #'invoke-transfer-error))
                                (future (error 'foo-error)))))
                (sleep 0.1)
                (repeat n
                  (submit-task
                   channel
                   (lambda ()
                     (handler-bind
                         ((foo-error (lambda (e)
                                       (declare (ignore e))
                                       (push-queue nil counter)
                                       (invoke-restart
                                        'store-value
                                        (queue-count counter)))))
                       (force future)))))
                (let ((results (loop repeat n
                                     collect (receive-result channel))))
                  (is (every #'= results (rest results))))))))

(base-test abort-future-test
  (handler-bind ((warning (lambda (w)
                            (when-let (r (find-restart 'muffle-warning w))
                              (invoke-restart r)))))
    (with-temp-kernel (2)
      (sleep 0.2)
      (let ((main-thread (current-thread)))
        (task-handler-bind
            ((foo-error (lambda (e)
                          (declare (ignore e))
                          ;; don't kill main thread
                          (unless (eq (current-thread) main-thread)
                            (invoke-abort-thread)))))
          (let ((future (future
                          (unless (eq (current-thread) main-thread)
                            (setf *error-output* (make-broadcast-stream)))
                          (error 'foo-error))))
            (sleep 0.1)
            (signals task-killed-error
              (force future))))))))

(base-test canceling-test
  (with-temp-kernel (2)
    (sleep 0.1)
    (let* ((a (promise))
           (filler1 (future (sleep 0.2)))
           (filler2 (future (sleep 0.2))))
      (declare (ignore filler1 filler2))
      (sleep 0.1)
      (let ((b (future (fulfill a 'foo))))
        (declare (ignore b))
        (sleep 0.2)
        (is (fulfilledp a))))
    (let* ((a (promise))
           (filler1 (future (sleep 0.6)))
           (filler2 (future (sleep 0.6))))
      (declare (ignore filler1 filler2))
      (sleep 0.1)
      (let ((b (future (fulfill a 'foo))))
        (sleep 0.2)
        (fulfill b 'nevermind)
        (sleep 0.2)
        (is (not (fulfilledp a)))))))

(base-test error-during-stealing-force-test
  (with-temp-kernel (2)
    ;; occupy workers
    (future (sleep 0.4))
    (future (sleep 0.4))
    (sleep 0.2)
    (let* ((call-count 0)
           (handle-count 0)
           (f (task-handler-bind ((foo-error
                                   (lambda (e)
                                     (invoke-restart 'transfer-error e))))
                (future
                  (incf call-count)
                  (error 'foo-error)))))
      (repeat 3
        (block top
          (handler-bind ((foo-error
                          (lambda (e)
                            (declare (ignore e))
                            (incf handle-count)
                            (return-from top))))
            (force f))))
      (is (= 1 call-count))
      (is (= 3 handle-count)))))

(base-test error-during-stealing-force-2-test
  (with-temp-kernel (2)
    ;; occupy workers
    (future (sleep 0.4))
    (future (sleep 0.4))
    (sleep 0.2)
    (let ((f (task-handler-bind ((foo-error
                                  (lambda (e)
                                    (declare (ignore e))
                                    (invoke-restart 'nine))))
               (future
                 (restart-case (error 'foo-error)
                   (nine () 9))))))
      (is (eql 9 (force f))))))

(base-test non-promise-test
  (dolist (obj (list 3 4.0 'foo (cons nil nil)))
    (is (fulfilledp obj))
    (is (eql obj (force obj)))
    (setf *memo* 11)
    (is (eql nil (fulfill obj (setf *memo* 22))))
    (is (eql 11 *memo*)))
  (let ((obj (chain 3)))
    (is (fulfilledp obj))
    (setf *memo* 11)
    (is (eql nil (fulfill obj (setf *memo* 22))))
    (is (eql 11 *memo*))))
