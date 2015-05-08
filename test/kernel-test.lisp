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

(full-test kernel-test
  (let ((channel (make-channel)))
    (mapcar (lambda (x) (submit-task channel (lambda () (* x x))))
            (list 5 6 7 8))
    (is (equal (list 25 36 49 64)
               (sort (collect-n 4 (receive-result channel)) '<)))))

(full-test no-kernel-test
  (let ((*kernel* nil))
    (signals no-kernel-error
      (submit-task (make-channel) (lambda ())))))

(base-test end-kernel-test
  (repeat 10
    (loop for n from 1 below 32
          do (with-temp-kernel (n)
               (is (= 1 1))))))

(full-test many-task-test
  (let ((channel (make-channel)))
    (repeat 1000
      (submit-task channel (lambda ()))
      (is (null (receive-result channel))))
    (repeat 1000
      (submit-task channel (lambda ())))
    (repeat 1000
      (is (null (receive-result channel))))
    (repeat 1000
      (let ((*task-priority* :low))
        (submit-task channel (lambda ())))
      (is (null (receive-result channel))))
    (repeat 1000
      (let ((*task-priority* :low))
        (submit-task channel (lambda ()))))
    (repeat 1000
      (is (null (receive-result channel))))))

#-lparallel.without-kill
(base-test kill-during-end-kernel-test
  (let* ((*kernel* (make-kernel 2))
         (kernel *kernel*)
         (out *standard-output*)
         (channel (make-channel))
         (handled (make-queue))
         (finished (make-queue)))
    (task-handler-bind ((error #'invoke-transfer-error))
      (submit-task channel (lambda ()
                             (setf *error-output* (make-broadcast-stream))
                             (infinite-loop))))
    (with-thread ()
      (block top
        (handler-bind ((task-killed-error
                        (lambda (e)
                          (declare (ignore e))
                          (push-queue t handled)
                          (return-from top))))
          (receive-result channel))))
    (sleep 0.2)
    (let ((thread (with-thread ()
                    (let ((*standard-output* out))
                      (let ((*kernel* kernel))
                        (end-kernel :wait t)
                        (push-queue t finished))))))
      (sleep 0.2)
      (is (null (peek-queue finished)))
      (is (eql 1 (kill-tasks :default)))
      (sleep 0.2)
      (is (eq t (peek-queue handled)))
      (is (eq t (peek-queue finished)))
      (is (not (null thread))))))

(full-test channel-capacity-test
  (let ((channel (make-channel :fixed-capacity 1)))
    (submit-task channel (lambda () 3))
    (submit-task channel (lambda () 4))
    (submit-task channel (lambda () 5))
    (is (equal '(3 4 5)
               ;; avoid sbcl warning
               (locally (declare (notinline sort))
                 (sort (list (receive-result channel)
                             (receive-result channel)
                             (receive-result channel))
                       #'<))))))

(full-test try-receive-test
  (let ((channel (make-channel)))
    (multiple-value-bind (a b) (try-receive-result channel)
      (is (null a))
      (is (null b)))
    (submit-task channel (lambda () 3))
    (sleep 0.1)
    (multiple-value-bind (a b) (try-receive-result channel)
      (is (= 3 a))
      (is (eq t b)))
    (multiple-value-bind (a b) (try-receive-result channel)
      (is (null a))
      (is (null b)))))

#-lparallel.without-bordeaux-threads-condition-wait-timeout
(full-test try-receive-timeout-test
  (let ((channel (make-channel)))
    (multiple-value-bind (a b) (try-receive-result channel :timeout 0.1)
      (is (null a))
      (is (null b)))
    (submit-task channel (lambda () 3))
    (sleep 0.1)
    (multiple-value-bind (a b) (try-receive-result channel :timeout 0.1)
      (is (= 3 a))
      (is (eq t b)))
    (multiple-value-bind (a b) (try-receive-result channel :timeout 0.1)
      (is (null a))
      (is (null b)))))

(full-test kernel-client-error-test
  (task-handler-bind ((client-error #'invoke-transfer-error))
    (let ((channel (make-channel)))
      (submit-task channel (lambda () (error 'client-error)))
      (signals client-error
        (receive-result channel))))
  (task-handler-bind
      ((error (lambda (e)
                (declare (ignore e))
                (invoke-restart 'transfer-error (make-condition 'foo-error)))))
    (let ((channel (make-channel)))
      (submit-task channel (lambda () (error 'client-error)))
      (signals foo-error
        (receive-result channel))))
  (task-handler-bind
      ((error (lambda (e)
                (declare (ignore e))
                (invoke-restart 'transfer-error 'foo-error))))
    (let ((channel (make-channel)))
      (submit-task channel (lambda () (error 'client-error)))
      (signals foo-error
        (receive-result channel)))))

(full-test user-restart-test
  (task-handler-bind
      ((foo-error (lambda (e)
                    (declare (ignore e))
                    (invoke-restart 'eleven))))
    (let ((channel (make-channel)))
      (submit-task channel (lambda ()
                             (restart-case (error 'foo-error)
                               (eleven () 11))))
      (is (eql 11 (receive-result channel)))))
  (task-handler-bind
      ((error (lambda (e)
                (declare (ignore e))
                (invoke-restart 'eleven))))
    (let ((channel (make-channel)))
      (submit-task channel (lambda ()
                             (restart-case (error 'foo-error)
                               (eleven () 11))))
      (is (eql 11 (receive-result channel))))))

(full-test error-cascade-test
  (task-handler-bind
      ((error (lambda (e)
                (invoke-restart 'transfer-error e))))
    (task-handler-bind
        ((error (lambda (e)
                  (declare (ignore e))
                  (error 'foo-error))))
      (let ((channel (make-channel)))
        (submit-task channel (lambda () (error 'client-error)))
        (signals foo-error
          (receive-result channel))))))

(base-test complex-handler-test
  (flet ((estr (e)
           (with-output-to-string (out)
             (write e :escape nil :stream out))))
    (let ((queue (make-queue)))
      (ignore-errors
        (handler-bind ((error (lambda (e)
                                (push-queue (cons 'a (estr e)) queue))))
          (handler-bind ((error (lambda (e)
                                  (push-queue (cons 'b (estr e)) queue)
                                  (error "Z"))))
            (handler-bind ((error (lambda (e)
                                    (push-queue (cons 'c (estr e)) queue)
                                    (error "Y"))))
              (handler-bind ((error (lambda (e)
                                      (push-queue (cons 'd (estr e)) queue))))
                (error "X"))))))
      (is (equal '((D . "X") (C . "X") (B . "Y") (A . "Z"))
                 (extract-queue queue))))

    (with-temp-kernel (2)
      (let ((queue (make-queue)))
        (task-handler-bind ((error #'invoke-transfer-error))
          (task-handler-bind ((error (lambda (e)
                                       (push-queue (cons 'a (estr e)) queue))))
            (task-handler-bind ((error (lambda (e)
                                         (push-queue (cons 'b (estr e)) queue)
                                         (error "Z"))))
              (task-handler-bind ((error (lambda (e)
                                           (push-queue (cons 'c (estr e)) queue)
                                           (error "Y"))))
                (task-handler-bind ((error (lambda (e)
                                             (push-queue (cons 'd (estr e))
                                                         queue))))
                  (submit-task (make-channel) #'error "X"))))))
        (is (equal '((D . "X") (C . "X") (B . "Y") (A . "Z"))
                   (loop repeat 4 collect (pop-queue queue))))))))

(base-test kernel-worker-context-test
  (with-temp-kernel (2 :context (lambda (run)
                                  (let ((*memo* 9))
                                    (funcall run))))
    (let ((channel (make-channel)))
      (setf *memo* 7)
      (submit-task channel (lambda () *memo*))
      (is (eql 9 (receive-result channel)))
      (is (eql 7 *memo*)))))

(base-test kernel-binding-test
  (unwind-protect
       (progn
         (end-kernel)
         (setf *kernel* (make-kernel 4))
         (let ((channel (make-channel)))
           (setf *memo* :main)
           (submit-task channel (lambda () (setf *memo* :worker) *memo*))
           (is (eq :worker (receive-result channel)))
           (is (eq :worker *memo*))))
    (end-kernel))
  (with-temp-kernel (4 :bindings (acons '*memo* :worker nil))
    (let ((node (assoc '*memo* (kernel-bindings))))
      (is (eq (cdr node) :worker)))
    (let ((channel (make-channel)))
      (setf *memo* :main)
      (submit-task channel (lambda () *memo*))
      (is (eq :worker (receive-result channel)))
      (is (eq :main *memo*)))))

(full-test kernel-var-test
  (let ((channel (make-channel)))
    (submit-task channel (lambda () *kernel*))
    (is (eq *kernel* (receive-result channel)))))

(base-test task-categories-test
  (with-temp-kernel (2)
    (is (notany #'identity (task-categories-running)))
    (let ((channel (make-channel)))
      (submit-task channel (lambda () (sleep 0.4)))
      (sleep 0.2)
      (is (eql 1 (count :default (task-categories-running))))))
  (with-temp-kernel (2)
    (let ((channel (make-channel)))
      (let ((*task-category* :foo))
        (submit-task channel (lambda () (sleep 0.4))))
      (sleep 0.2)
      (is (eql 1 (count :foo (task-categories-running))))))
  (with-temp-kernel (2)
    (let ((channel (make-channel)))
      (let ((*task-category* 999))
        (submit-task channel (lambda () (sleep 0.4))))
      (sleep 0.2)
      (is (eql 1 (count 999 (task-categories-running))))))
  (with-temp-kernel (2)
    (let ((channel (make-channel)))
      (let ((*task-category* :foo))
        (submit-task channel (lambda () (sleep 0.4)))
        (submit-task channel (lambda () (sleep 0.4))))
      (sleep 0.2)
      (is (eql 2 (count :foo (task-categories-running)))))))

(base-test no-kernel-restart-test
  (let ((*kernel* nil))
    (unwind-protect
         (let ((flag nil))
           (handler-bind
               ((no-kernel-error
                 (lambda (c)
                   (setf flag :called)
                   (invoke-restart (find-restart 'make-kernel c) 3))))
             (let ((channel (make-channel)))
               (submit-task channel (lambda (x) (* x x)) 3)
               (is (= 9 (receive-result channel))))
             (is (= 3 (kernel-worker-count)))
             (is (eq :called flag))))
      (end-kernel))))

(base-test kernel-warnings-test
  (let ((*error-output* (make-string-output-stream)))
    (with-temp-kernel (3)
      (is (zerop (length (get-output-stream-string *error-output*))))
      (let ((channel (make-channel)))
        (submit-task channel (lambda () (warn "blah")))
        (receive-result channel))
      (is (search "blah" (get-output-stream-string *error-output*))))))

(full-test handler-bind-test
  (task-handler-bind
      ((foo-error (lambda  (e)
                    (declare (ignore e))
                    (invoke-restart 'double-me 3))))
    (let ((channel (make-channel)))
      (repeat 3
        (submit-task channel (lambda ()
                               (restart-case (error 'foo-error)
                                 (double-me (x)
                                   ;; clisp warns unless interactive is given
                                   :interactive (lambda ())
                                   (* 2 x))))))
      (is (equal '(6 6 6)
                 (collect-n 3 (receive-result channel)))))))

(full-test aborted-worker-test
  (task-handler-bind ((foo-error (lambda (e)
                                   (declare (ignore e))
                                   (invoke-abort-thread))))
    (let ((channel (make-channel)))
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

(base-test active-worker-replacement-test
  (with-thread-count-check
    (with-temp-kernel (2)
      (is (all-workers-alive-p))
      (task-handler-bind ((foo-error (lambda (e)
                                       (declare (ignore e))
                                       (invoke-abort-thread))))
        (let ((channel (make-channel)))
          (submit-task channel (lambda ()
                                 (setf *error-output* (make-broadcast-stream))
                                 (error 'foo-error)))
          (signals task-killed-error
            (receive-result channel))))
      (is (all-workers-alive-p)))))

#-lparallel.without-kill
(base-test sleeping-worker-replacement-test
  (with-thread-count-check
    (with-temp-kernel (2 :bindings (list (cons '*error-output*
                                               (make-broadcast-stream))))
      (is (all-workers-alive-p))
      (destroy-thread
       (lparallel.kernel::thread
        (aref (lparallel.kernel::workers *kernel*) 0)))
      (is (all-workers-alive-p))
      (destroy-thread
       (lparallel.kernel::thread
        (aref (lparallel.kernel::workers *kernel*) 0)))
      (destroy-thread
       (lparallel.kernel::thread
        (aref (lparallel.kernel::workers *kernel*) 1)))
      (is (all-workers-alive-p)))))

(define-condition foo-condition () ())

(full-test non-error-condition-test
  (let ((result nil))
    (task-handler-bind ((foo-condition (lambda (c)
                                         (declare (ignore c))
                                         (setf result :called))))
      (let ((channel (make-channel)))
        (submit-task channel (lambda ()
                               (signal 'foo-condition)))
        (receive-result channel)))
    (is (eq :called result))))

#-lparallel.without-kill
(base-test custom-kill-task-test
  (with-thread-count-check
    (with-temp-kernel (2)
      (let ((channel (make-channel)))
        (let ((*task-category* 'blah))
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

#-lparallel.without-kill
(base-test default-kill-task-test
  (with-thread-count-check
    (with-temp-kernel (2)
      (let ((channel (make-channel)))
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

(base-test submit-timeout-test
  (with-temp-kernel (2)
    (let ((channel (make-channel)))
      (declare (notinline submit-timeout))
      (submit-timeout channel 0.1 'timeout)
      (submit-task channel (lambda () 3))
      (is (eql 3 (receive-result channel)))
      (is (eq 'timeout (receive-result channel))))))

#-lparallel.without-kill
(base-test cancel-timeout-test
  (with-temp-kernel (2)
    (locally (declare (notinline submit-timeout cancel-timeout))
      (let* ((channel (make-channel))
             (timeout (submit-timeout channel 999 'timeout)))
        (sleep 0.2)
        (cancel-timeout timeout 'a)
        (is (eq 'a (receive-result channel)))))))

#-lparallel.without-kill
(base-test kill-timeout-test
  (with-temp-kernel (2)
    (locally (declare (notinline submit-timeout))
      (let* ((channel (make-channel))
             (timeout (submit-timeout channel 999 'timeout)))
        (sleep 0.2)
        (lparallel.kernel::with-timeout-slots (lparallel.kernel::thread) timeout
          (destroy-thread lparallel.kernel::thread))
        (signals task-killed-error
          (receive-result channel))))))

(define-condition foo-condition-2 (condition) ())

(full-test signaling-after-signal-test
  (let ((q (make-queue)))
    (task-handler-bind ((foo-condition-2 (lambda (c)
                                           (declare (ignore c))
                                           (push-queue 'outer q))))
      (task-handler-bind ((foo-condition (lambda (c)
                                           (declare (ignore c))
                                           (push-queue 'inner q)
                                           (signal 'foo-condition-2))))
        (let ((channel (make-channel)))
          (submit-task channel (lambda () (signal 'foo-condition)))
          (receive-result channel))))
    (is (equal '(inner outer)
               (extract-queue q)))))

(base-test task-handler-bind-syntax-test
  (signals error
    (macroexpand '(task-handler-bind ((())))))
  (signals error
    (macroexpand '(task-handler-bind (()))))
  (signals error
    (macroexpand '(task-handler-bind ((x)))))
  (signals error
    (macroexpand '(task-handler-bind ((x y z))))))

(full-test print-kernel-test
  (is (plusp (length (with-output-to-string (s)
                       (print *kernel* s))))))

(base-test end-kernel-wait-test
  (with-thread-count-check
    (let ((*kernel* (make-kernel 3)))
      (unwind-protect
           (let ((channel (make-channel)))
             (submit-task channel (lambda () (sleep 1))))
        (is (eql 3 (length (end-kernel :wait t))))))))

(base-test steal-work-test
  (with-temp-kernel (2)
    (let ((channel (make-channel)))
      (submit-task channel (lambda () (sleep 0.4)))
      (submit-task channel (lambda () (sleep 0.4)))
      (sleep 0.1)
      (let ((execp nil))
        (submit-task channel (lambda () (setf execp t)))
        (sleep 0.1)
        (is (eq t (lparallel.kernel::steal-work
                   *kernel*
                   lparallel.kernel::*worker*)))
        (is (eq t execp))
        (is (eq nil (lparallel.kernel::steal-work
                     *kernel*
                     lparallel.kernel::*worker*))))))
  (with-temp-kernel (2)
    (let ((channel (make-channel)))
      (submit-task channel (lambda () (sleep 0.2)))
      (submit-task channel (lambda () (sleep 0.2)))
      (sleep 0.1)
      (is (eq nil (lparallel.kernel::steal-work
                   *kernel*
                   lparallel.kernel::*worker*))))))

(base-test kernel-store-value-test
  (unwind-protect
       (handler-bind ((no-kernel-error
                       (lambda (e)
                         (declare (ignore e))
                         (invoke-restart 'store-value
                                         (make-kernel 2)))))
         (let ((channel (make-channel)))
           (submit-task channel 'identity 3)
           (is (= 3 (receive-result channel)))))
    (end-kernel)))

#-lparallel.without-kill
(base-test reject-kill-nil-test
  (with-temp-kernel (2)
    (let ((channel (make-channel)))
      (submit-task channel (lambda ()
                             (setf *error-output* (make-broadcast-stream))
                             (sleep 999)))
      (sleep 0.2)
      (signals error
        (kill-tasks nil))
      (= 1 (kill-tasks :default)))))

#-lparallel.without-kill
(full-test worker-suicide-test
  (let ((channel (make-channel)))
    (submit-task channel (lambda ()
                           (setf *error-output* (make-broadcast-stream))
                           (kill-tasks :default)))
    (signals task-killed-error
      (receive-result channel)))
  (let ((channel (make-channel))
        (*task-category* 'foo))
    (submit-task channel (lambda ()
                           (setf *error-output* (make-broadcast-stream))
                           (kill-tasks 'foo)))
    (signals task-killed-error
      (receive-result channel))))

(full-test submit-after-end-kernel-test
  (let ((channel (make-channel)))
    (end-kernel :wait t)
    (signals error
      (submit-task channel (lambda ())))))

(base-test double-end-kernel-test
  (let* ((kernel (make-kernel 2))
         (*kernel* kernel))
    (end-kernel :wait t)
    (let ((*kernel* kernel))
      (end-kernel :wait t)))
  ;; got here without an error
  (is (= 1 1)))

(base-test kernel-reader-test
  (setf *memo* nil)
  (let ((context (lambda (worker-loop)
                   (let ((*memo* 3))
                     (funcall worker-loop)))))
    (with-temp-kernel (2 :name "foo"
                         :bindings `((*blah* . 99))
                         :context context)
      (let ((channel (make-channel)))
        (submit-task channel (lambda ()
                               (declare (special *blah*))
                               (list *memo* *blah*)))
        (is (equal '(3 99) (receive-result channel))))
      (is (string-equal "foo" (kernel-name)))
      (is (equal '((*blah* . 99)) (kernel-bindings)))
      (is (eq context (kernel-context))))))

(defun aborting-context (worker-loop)
  (declare (ignore worker-loop))
  (invoke-abort-thread))

(defun non-funcalling-context (worker-loop)
  (declare (ignore worker-loop)))

(base-test context-error-test
  (dolist (n '(1 2 4 8))
    (with-thread-count-check
      (signals kernel-creation-error
        (make-kernel n :context #'aborting-context)))))

(base-test non-funcalling-context-test
  (dolist (n '(1 2 4 8))
    (with-thread-count-check
      (signals kernel-creation-error
        (make-kernel n :context 'non-funcalling-context)))))

(base-test nonexistent-context-test
  (with-thread-count-check
    (signals error
      (make-kernel 1 :context 'nonexistent-function))))

(base-test broadcast-test
  (setf *memo* 0)
  (dolist (n '(1 2 3 4 7 8 15 16))
    (with-temp-kernel (n :bindings '((*memo* . 1)))
      (is (= 0 *memo*))
      (let ((channel (make-channel)))
        (repeat 100 (submit-task channel (lambda () *memo*)))
        (repeat 100 (is (= 1 (receive-result channel)))))
      (is (every (lambda (x) (= x 1))
                 (broadcast-task (lambda () *memo*))))
      (let ((channel (make-channel)))
        (repeat (kernel-worker-count)
          (submit-task channel #'sleep 0.2)))
      (is (every (lambda (x) (= x 99))
                 (broadcast-task (lambda () (setf *memo* 99)))))
      (let ((channel (make-channel)))
        (repeat 1000 (submit-task channel (lambda ()))))
      (is (every (lambda (x) (= x 99))
                 (broadcast-task (lambda () (setf *memo* 99)))))
      (is (every (lambda (x) (= x 99))
                 (broadcast-task (lambda () (setf *memo* 99)))))
      (is (= 0 *memo*))
      (let ((channel (make-channel)))
        (repeat 100 (submit-task channel (lambda () *memo*)))
        (repeat 100 (is (= 99 (receive-result channel)))))
      (let ((channel (make-channel)))
        (repeat 1000 (submit-task channel (lambda ()))))
      (is (every (lambda (x) (= x 99))
                 (broadcast-task (lambda () *memo*))))
      (is (every (lambda (x) (= x 99))
                 (broadcast-task (lambda () *memo*))))
      (is (every (lambda (x) (= x 5))
                 (broadcast-task #'+ 2 3))))))

(full-test broadcast-error-test
  (let ((*kernel* nil))
    (signals no-kernel-error
      (broadcast-task (lambda ()))))
  (signals error
    (broadcast-task 3))
  (signals error
    (broadcast-task "foo"))
  (task-handler-bind ((error #'invoke-transfer-error))
    (signals foo-error
      (broadcast-task #'error 'foo-error))
    (let ((channel (make-channel)))
      (submit-task channel (lambda () (broadcast-task (lambda ()))))
      (signals error
        (receive-result channel)))
    (signals error
      (broadcast-task (lambda () (broadcast-task (lambda ())))))))

(full-test worker-index-test
  (is (null (kernel-worker-index)))
  (let ((channel (make-channel)))
    (repeat 1000
      (submit-task channel #'kernel-worker-index))
    (repeat 1000
      (let ((x (receive-result channel)))
        (is (and (>= x 0)
                 (< x (kernel-worker-count)))))))
  (loop for i across (sort (broadcast-task #'kernel-worker-index) #'<)
        for j from 0
        do (is (= i j))))

;;;; check for messed up imports

(defun packages-matching (string)
  (remove-if-not (lambda (package)
                   (search string (package-name package) :test #'equalp))
                 (list-all-packages)))

(defun assert-internal-symbols-not-imported (&key own-packages
                                             third-party-packages)
  (let ((third-party-packages (mapcar #'find-package third-party-packages)))
    (dolist (own-package own-packages)
      (do-symbols (symbol own-package)
        (when-let (third-party-package (find (symbol-package symbol)
                                             third-party-packages))
          (when (eq :internal (nth-value 1 (find-symbol (symbol-name symbol)
                                                        third-party-package)))
            (error "Internal symbol ~s was imported into ~a."
                   symbol (package-name own-package))))))))

(base-test package-test
  (assert-internal-symbols-not-imported
   :own-packages (packages-matching "lparallel")
   :third-party-packages '(#:alexandria #:bordeaux-threads))
  (is t))
