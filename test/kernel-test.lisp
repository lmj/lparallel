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

(lp-test kernel-test
  (let1 channel (make-channel)
    (mapcar (lambda (x) (submit-task channel (lambda () (* x x))))
            (list 5 6 7 8))
    (is (equal (list 25 36 49 64)
               (sort (collect-n 4 (receive-result channel)) '<)))))

(lp-test no-kernel-test
  (let1 *kernel* nil
    (signals no-kernel-error
      (submit-task (make-channel) (lambda ())))))

(lp-test channel-capacity-test
  (let1 channel (make-channel 10)
    (submit-task channel (lambda () 3))
    (submit-task channel (lambda () 4))
    (is (equal '(3 4)
               (sort (list  (receive-result channel)
                            (receive-result channel))
                     #'<)))))

(lp-test kernel-client-error-test
  (task-handler-bind
      ((client-error (lambda (e)
                       (invoke-restart 'transfer-error e))))
    (let1 channel (make-channel)
      (submit-task channel (lambda () (error 'client-error)))
      (signals client-error
        (receive-result channel))))
  (task-handler-bind
      ((error (lambda (e)
                (declare (ignore e))
                (invoke-restart 'transfer-error (make-condition 'foo-error)))))
    (let1 channel (make-channel)
      (submit-task channel (lambda () (error 'client-error)))
      (signals foo-error
        (receive-result channel))))
  (task-handler-bind
      ((error (lambda (e)
                (declare (ignore e))
                (invoke-restart 'transfer-error 'foo-error))))
    (let1 channel (make-channel)
      (submit-task channel (lambda () (error 'client-error)))
      (signals foo-error
        (receive-result channel)))))

(lp-test user-restart-test
  (task-handler-bind
      ((foo-error (lambda (e)
                    (declare (ignore e))
                    (invoke-restart 'eleven))))
    (let1 channel (make-channel)
      (submit-task channel (lambda ()
                             (restart-case (error 'foo-error)
                               (eleven () 11))))
      (is (eql 11 (receive-result channel)))))
  (task-handler-bind
      ((error (lambda (e)
                (declare (ignore e))
                (invoke-restart 'eleven))))
    (let1 channel (make-channel)
      (submit-task channel (lambda ()
                             (restart-case (error 'foo-error)
                               (eleven () 11))))
      (is (eql 11 (receive-result channel))))))

(lp-test error-cascade-test
  (task-handler-bind
      ((error (lambda (e)
                (invoke-restart 'transfer-error e))))
    (task-handler-bind
        ((error (lambda (e)
                  (declare (ignore e))
                  (error 'foo-error))))
      (let1 channel (make-channel)
        (submit-task channel (lambda () (error 'client-error)))
        (signals foo-error
          (receive-result channel))))))

(lp-base-test kernel-worker-context-test
  (with-new-kernel (2 :worker-context (lambda (run)
                                        (let1 *memo* 9
                                          (funcall run))))
    (let1 channel (make-channel)
      (setf *memo* 7)
      (submit-task channel (lambda () *memo*))
      (is (eql 9 (receive-result channel)))
      (is (eql 7 *memo*)))))

(lp-base-test kernel-binding-test
  (unwind-protect
       (progn
         (end-kernel)
         (setf *kernel* (make-kernel 4))
         (let1 channel (make-channel)
           (setf *memo* :main)
           (submit-task channel (lambda () (setf *memo* :worker) *memo*))
           (is (eq :worker (receive-result channel)))
           (is (eq :worker *memo*))))
    (end-kernel))
  (with-new-kernel (4 :bindings (acons '*memo* :worker nil))
    (let1 channel (make-channel)
      (setf *memo* :main)
      (submit-task channel (lambda () *memo*))
      (is (eq :worker (receive-result channel)))
      (is (eq :main *memo*)))))

(lp-base-test no-kernel-restart-test
  (let1 *kernel* nil
    (unwind-protect
         (let1 flag nil
           (handler-bind
               ((no-kernel-error
                 (lambda (c)
                   (setf flag :called)
                   (invoke-restart (find-restart 'make-kernel c) 3))))
             (let1 channel (make-channel)
               (submit-task channel (lambda (x) (* x x)) 3)
               (is (= 9 (receive-result channel))))
             (is (= 3 (kernel-worker-count)))
             (is (eq :called flag))))
      (end-kernel))))

(lp-base-test kernel-warnings-test
  (let1 *error-output* (make-string-output-stream)
    (with-new-kernel (3)
      (is (zerop (length (get-output-stream-string *error-output*))))
      (let1 channel (make-channel)
        (submit-task channel (lambda () (warn "blah")))
        (receive-result channel))
      (is (search "blah" (get-output-stream-string *error-output*))))))

(lp-test handler-bind-test
  (task-handler-bind
      ((foo-error (lambda  (e)
                    (declare (ignore e))
                    (invoke-restart 'double-me 3))))
    (let ((channel (make-channel)))
      (repeat 3
        (submit-task channel (lambda ()
                               (restart-case (error 'foo-error)
                                 (double-me (x) (* 2 x))))))
      (is (equal '(6 6 6)
                 (collect-n 3 (receive-result channel)))))))

(lp-test killed-worker-test
  (task-handler-bind ((foo-error (lambda (e)
                                   (declare (ignore e))
                                   (invoke-abort-thread))))
    (let1 channel (make-channel)
      (submit-task channel (lambda ()
                             (setf *error-output* (make-broadcast-stream))
                             (restart-case (error 'foo-error)
                               (eleven () 11))))
      (signals task-killed-error
        (receive-result channel)))))

(defun all-workers-alive-p ()
  (sleep 0.2)
  (every #'bordeaux-threads:thread-alive-p
         (map 'list
              #'lparallel.kernel::thread
              (lparallel.kernel::workers *kernel*))))

(lp-base-test active-worker-replacement-test
  (with-thread-count-check
    (with-new-kernel (2)
      (is (all-workers-alive-p))
      (task-handler-bind ((foo-error (lambda (e)
                                       (declare (ignore e))
                                       (invoke-abort-thread))))
        (let1 channel (make-channel)
          (submit-task channel (lambda ()
                                 (setf *error-output* (make-broadcast-stream))
                                 (error 'foo-error)))
          (signals task-killed-error
            (receive-result channel))))
      (is (all-workers-alive-p)))))

(lp-base-test sleeping-worker-replacement-test
  (with-thread-count-check
    (with-new-kernel (2 :bindings (list (cons '*error-output*
                                              (make-broadcast-stream))))
      (is (all-workers-alive-p))
      (bordeaux-threads:destroy-thread 
       (lparallel.kernel::thread
        (aref (lparallel.kernel::workers *kernel*) 0)))
      (is (all-workers-alive-p))
      (bordeaux-threads:destroy-thread 
       (lparallel.kernel::thread
        (aref (lparallel.kernel::workers *kernel*) 0)))
      (bordeaux-threads:destroy-thread 
       (lparallel.kernel::thread
        (aref (lparallel.kernel::workers *kernel*) 1)))
      (is (all-workers-alive-p)))))

(define-condition foo-condition () ())

(lp-test non-error-condition-test
  (let1 result nil
    (task-handler-bind ((foo-condition (lambda (c)
                                         (declare (ignore c))
                                         (setf result :called))))
      (let1 channel (make-channel)
        (submit-task channel (lambda ()
                               (signal 'foo-condition)))
        (receive-result channel)))
    (is (eq :called result))))

(defparameter *nil* nil)
(defun infinite-loop () (loop :until *nil*))

#-abcl
(lp-base-test custom-kill-task-test
  (with-thread-count-check
    (with-new-kernel (2)
      (let1 channel (make-channel)
        (let1 *task-category* 'blah
          (submit-task channel (lambda ()
                                 (setf *error-output* (make-broadcast-stream))
                                 (infinite-loop)))
          (submit-task channel (lambda ()
                                 (setf *error-output* (make-broadcast-stream))
                                 (infinite-loop))))
        (sleep 0.2)
        (submit-task channel (lambda () 'survived))
        (sleep 0.2)
        (kill-tasks 'blah)
        (sleep 0.2)
        (let ((errors nil)
              (regulars nil))
          (repeat 3
            (handler-case (push (receive-result channel) regulars)
              (task-killed-error (e)
                (push e errors))))
          (is (= 2 (length errors)))
          (is (equal '(survived) regulars)))))))

#-abcl
(lp-base-test default-kill-task-test
  (with-thread-count-check
    (with-new-kernel (2)
      (let1 channel (make-channel)
        (submit-task channel (lambda ()
                               (setf *error-output* (make-broadcast-stream))
                               (infinite-loop)))
        (submit-task channel (lambda ()
                               (setf *error-output* (make-broadcast-stream))
                               (infinite-loop)))
        (sleep 0.2)
        (submit-task channel (lambda () 'survived))
        (sleep 0.2)
        (kill-tasks *task-category*)
        (sleep 0.2)
        (let ((errors nil)
              (regulars nil))
          (repeat 3
            (handler-case (push (receive-result channel) regulars)
              (task-killed-error (e)
                (push e errors))))
          (is (= 2 (length errors)))
          (is (equal '(survived) regulars)))))))

(lp-base-test submit-timeout-test
  (with-new-kernel (2)
    (let1 channel (make-channel)
      (submit-timeout channel 0.1 'timeout)
      (submit-task channel (lambda () 3))
      (is (eql 3 (receive-result channel)))
      (is (eq 'timeout (receive-result channel))))))

(lp-base-test cancel-timeout-test
  (with-new-kernel (2)
    (let* ((channel (make-channel))
           (timeout (submit-timeout channel 999 'timeout)))
      (sleep 0.2)
      (cancel-timeout timeout 'a)
      (is (eq 'a (receive-result channel))))))

(lp-base-test kill-timeout-test
  (with-new-kernel (2)
    (let* ((channel (make-channel))
           (timeout (submit-timeout channel 999 'timeout)))
      (sleep 0.2)
      (lparallel.kernel::with-timeout-slots (lparallel.kernel::thread) timeout
        (bordeaux-threads:destroy-thread lparallel.kernel::thread))
      (signals task-killed-error
        (receive-result channel)))))

(define-condition foo-condition-2 (condition) ())

(lp-test signaling-after-signal-test
  (let1 q (make-queue)
    (task-handler-bind ((foo-condition-2 (lambda (c)
                                           (declare (ignore c))
                                           (push-queue 'outer q))))
      (task-handler-bind ((foo-condition (lambda (c)
                                           (declare (ignore c))
                                           (push-queue 'inner q)
                                           (signal 'foo-condition-2))))
        (let1 channel (make-channel)
          (submit-task channel (lambda () (signal 'foo-condition)))
          (receive-result channel))))
    (is (equal '(inner outer)
               (extract-queue q)))))

(lp-test print-kernel-test
  (is (plusp (length (with-output-to-string (s)
                       (print *kernel* s))))))

(lp-base-test end-kernel-wait-test
  (with-thread-count-check
    (let1 *kernel* (make-kernel 3)
      (unwind-protect
           (let1 channel (make-channel)
             (submit-task channel (lambda () (sleep 1))))
        (is (eql 3 (length (end-kernel :wait t))))))))
