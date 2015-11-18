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

(full-test basic-ptree-test
  (ptree ((a (b c) (- b c))
          (b ()    3)
          (c ()    4))
    (is (= a (- 3 4)))
    (is (= b 3))
    (is (= c 4))))

(full-test basic-ptree-2-test
  (ptree ((area   (width height) (* width height))
          (width  (border)       (+ 7 (* 2 border)))
          (height (border)       (+ 5 (* 2 border)))
          (border ()             1))
    (is (= (* (+ 7 (* 2 1)) (+ 5 (* 2 1)))
           area))
    (is (= (+ 7 (* 2 1))
           width))
    (is (= (+ 5 (* 2 1))
           height))
    (is (= 1
           border))))

(full-test basic-ptree-fn-test
  (let ((tree (make-ptree)))
    (ptree-fn 'area   '(width height) (lambda (w h) (* w h))       tree)
    (ptree-fn 'width  '(border)       (lambda (b)   (+ 7 (* 2 b))) tree)
    (ptree-fn 'height '(border)       (lambda (b)   (+ 5 (* 2 b))) tree)
    (ptree-fn 'border '()             (lambda ()    1)             tree)
    (is (= (* (+ 7 (* 2 1)) (+ 5 (* 2 1)))
           (call-ptree 'area tree)))
    (is (= (+ 7 (* 2 1))
           (call-ptree 'width tree)))
    (is (= (+ 5 (* 2 1))
           (call-ptree 'height tree)))
    (is (= 1
           (call-ptree 'border tree)))))

(full-test ptree-no-double-compute-test
  (let ((tree (make-ptree)))
    (setf *memo* 0)
    (ptree-fn 'f '(x) (lambda (x) (* x x)) tree)
    (ptree-fn 'x '()  (lambda () (incf *memo*) 5) tree)
    (is (= 25 (call-ptree 'f tree)))
    (is (= 1 *memo*))
    (is (= 25 (call-ptree 'f tree)))
    (is (= 1 *memo*))
    (is (= 5 (call-ptree 'x tree)))
    (is (= 1 *memo*))))

(full-test ptree-lone-fn-test
  (ptree ((lone () 7))
    (is (= 7 lone))))

(full-test ptree-missing-node-test
  (let ((tree (make-ptree)))
    (ptree-fn 'f '(x) (lambda (x) (* x x)) tree)
    (signals ptree-undefined-function-error
      (call-ptree 'g tree))))

(full-test ptree-unknown-node-test
  (let ((signaledp nil))
    (handler-case
        (task-handler-bind ((error #'invoke-transfer-error))
          (ptree ((x () 3)
                  (y () 4)
                  (z (x h) (* x h)))
            (list x y z)))
      (error (err)
        (setf signaledp t)
        (is (eq 'ptree-undefined-function-error (type-of err)))
        (let ((id (lparallel.ptree::ptree-error-id err))
              (refs (lparallel.ptree::ptree-error-refs err)))
          (is (equal 'h id))
          (is (equal '(z)
                     ;; avoid sbcl warning
                     (locally (declare (notinline sort))
                       (sort (copy-list refs) #'string<)))))))
    (is (not (null signaledp)))))

(full-test missing-ptree-function-test
  (let ((tree (make-ptree)))
    (ptree-fn 'area   '(width height) (lambda (w h) (* w h))       tree)
    (ptree-fn 'width  '(border)       (lambda (b)   (+ 7 (* 2 b))) tree)
    (ptree-fn 'height '(border)       (lambda (b)   (+ 5 (* 2 b))) tree)
    (handler-case (check-ptree tree)
      (error (err)
        (is (eq 'ptree-undefined-function-error (type-of err)))
        (let ((id (lparallel.ptree::ptree-error-id err))
              (refs (lparallel.ptree::ptree-error-refs err)))
          (is (equal 'border id))
          (is (equal '(height width)
                     ;; avoid sbcl warning
                     (locally (declare (notinline sort))
                       (sort (copy-list refs) #'string<)))))))))

(full-test ptree-redefinition-test
  (signals ptree-redefinition-error
    (let ((tree (make-ptree)))
      (ptree-fn 'foo () (lambda () 3) tree)
      (ptree-fn 'foo () (lambda () 4) tree))))

(full-test lambda-list-keywords-in-ptree-test
  (signals ptree-lambda-list-keyword-error
    (eval '(ptree ((foo (x &optional y) (list x y))))))
  (signals ptree-lambda-list-keyword-error
    (eval '(ptree ((foo (x &REST y) (list x y)))))))

(full-test error-inside-ptree-function-test
  (let ((memo (make-queue)))
    (task-handler-bind ((foo-error (lambda (e)
                                     (push-queue e memo)
                                     (invoke-restart 'transfer-error e))))
      (let ((tree (make-ptree)))
        (ptree-fn 'root '(child) (lambda (x) x) tree)
        (ptree-fn 'child () (lambda () (error 'foo-error)) tree)
        (let ((err nil))
          (handler-case (call-ptree 'root tree)
            (error (result) (setf err result)))
          (is (not (null err)))
          (is (eq 'foo-error (type-of err))))))
    (is (= 1 (queue-count memo)))
    (is (typep (pop-queue memo) 'foo-error))))

(defmacro/once for-range ((var &once pair) &body body)
  `(loop for ,var from (first ,pair) to (second ,pair)
         do (progn ,@body)))

(full-test grind-ptree-test
  (let ((level-range '(1 5))
        (children-range '(1 5))
        (main-iterations 3)
        (root (gensym))
        (count nil))
    (flet ((generate-ptree (num-levels num-children tree)
             (labels ((pick-names ()
                        (collect-n (1+ (random num-children))
                          (gensym)))
                      (build-tree (parent children level)
                        (ptree-fn parent children #'+ tree)
                        (dolist (child children)
                          (cond ((< level num-levels)
                                 (build-tree child (pick-names) (1+ level)))
                                (t
                                 (incf count)
                                 (ptree-fn child () (lambda () 1) tree))))))
               (build-tree root (pick-names) 0))))
      (for-range (num-levels level-range)
        (for-range (num-children children-range)
          (repeat main-iterations
            (let ((tree (make-ptree)))
              (setf count 0)
              (generate-ptree num-levels num-children tree)
              (is (eql count (call-ptree root tree))))))))))

(base-test ptree-node-kernel-test
  (let ((*ptree-node-kernel* (make-kernel 2)))
    (unwind-protect
         (with-temp-kernel (1)
           (is (equal
                '(63 9 7 1)
                (ptree ((area   (width height) (* width height))
                        (width  (border)       (+ 7 (* 2 border)))
                        (height (border)       (+ 5 (* 2 border)))
                        (border ()             (let ((channel (make-channel)))
                                                 (submit-task
                                                  channel (lambda () 1))
                                                 (receive-result channel))))
                  ;; will hang without separate node kernel
                  (list area width height border)))))
      (let ((*kernel* *ptree-node-kernel*))
        (end-kernel)))))

(full-test ptree-node-id-test
  (let ((tree   (make-ptree))
        (area   (cons nil nil))
        (width  9999)
        (height 'height)
        (border (cons nil nil)))
    (ptree-fn area   (list width height) (lambda (w h) (* w h))       tree)
    (ptree-fn width  (list border)       (lambda (b)   (+ 7 (* 2 b))) tree)
    (ptree-fn height (list border)       (lambda (b)   (+ 5 (* 2 b))) tree)
    (ptree-fn border '()                 (lambda ()    1)             tree)
    (is (= 63 (call-ptree area tree)))
    (is (= 9  (call-ptree width tree)))
    (is (= 7  (call-ptree height tree)))
    (is (= 1  (call-ptree border tree)))))

(full-test ptree-basic-restart-test
  (task-handler-bind ((foo-error (lambda (e)
                                   (declare (ignore e))
                                   (invoke-restart 'nine))))
    (ptree ((result () (restart-case
                           (error 'foo-error)
                         (nine () 9))))
      (is (= 9 result)))))

(full-test ptree-restart-test
  (task-handler-bind ((foo-error (lambda (e)
                                   (declare (ignore e))
                                   (invoke-restart 'nine))))
    (ptree ((area (width height) (* width height))
            (width () 3)
            (height () (restart-case
                           (error 'foo-error)
                         (nine () 9))))
      (is (= 27 area))
      (is (= 3 width))
      (is (= 9 height)))
    (ptree ((area (width height) (* width height))
            (height () (restart-case
                           (error 'foo-error)
                         (nine () 9)))
            (width () 3))
      (is (= 27 area))
      (is (= 3 width))
      (is (= 9 height)))))

(full-test ptree-transfer-error-test
  (task-handler-bind ((foo-error #'invoke-transfer-error))
    (ptree ((area (width height) (* width height))
            (width () 3)
            (height () (restart-case
                           (error 'foo-error)
                         (nine () 9))))
      (signals foo-error
        area)
      (signals foo-error
        height)
      (is (= 3 width)))
    (ptree ((area (width height) (* width height))
            (width () 3)
            (height () (restart-case
                           (error 'foo-error)
                         (nine () 9))))
      (signals foo-error
        height)
      (signals foo-error
        area)
      (is (= 3 width)))))

#-lparallel.without-kill
(base-test ptree-kill-test
  (let ((memo (make-queue))
        (tree (make-ptree)))
    (ptree-fn 'inf '() #'infinite-loop tree)
    (with-temp-kernel
        (2 :bindings `((*error-output* . (make-broadcast-stream))))
      (with-thread (:bindings `((*kernel* . ,*kernel*)))
        (handler-case
            (call-ptree 'inf tree)
          (error (e)
            (push-queue e memo))))
      (sleep 0.2)
      (is (= 1 (kill-tasks :default)))
      (sleep 0.2)
      (is (= 1 (queue-count memo)))
      (is (typep (pop-queue memo) 'task-killed-error))
      (signals task-killed-error
        (call-ptree 'inf tree))
      (signals task-killed-error
        (call-ptree 'inf tree)))))

#-lparallel.without-kill
(base-test second-ptree-kill-test
  (let ((memo (make-queue))
        (tree (make-ptree)))
    (ptree-fn 'area '(width height) (lambda (w h) (* w h)) tree)
    (ptree-fn 'height '() #'infinite-loop tree)
    (ptree-fn 'width '() (constantly 9) tree)
    (with-temp-kernel
        (2 :bindings `((*error-output* . (make-broadcast-stream))))
      (with-thread (:bindings `((*kernel* . ,*kernel*)))
        (handler-case
            (call-ptree 'area tree)
          (error (e)
            (push-queue e memo))))
      (sleep 0.2)
      (is (= 1 (kill-tasks :default)))
      (sleep 0.2)
      (is (= 1 (queue-count memo)))
      (is (typep (pop-queue memo) 'task-killed-error))
      (signals task-killed-error
        (call-ptree 'area tree))
      (signals task-killed-error
        (call-ptree 'height tree))
      (is (= 9 (call-ptree 'width tree))))))

#-lparallel.without-kill
(base-test third-ptree-kill-test
  (let ((memo (make-queue))
        (tree (make-ptree)))
    (ptree-fn 'inf '(five)
              (lambda (x)
                (declare (ignore x))
                (infinite-loop))
              tree)
    (ptree-fn 'five '() (constantly 5) tree)
    (with-temp-kernel
        (2 :bindings `((*error-output* . (make-broadcast-stream))))
      (with-thread (:bindings `((*kernel* . ,*kernel*)))
        (handler-case
            (call-ptree 'inf tree)
          (error (e)
            (push-queue e memo))))
      (sleep 0.2)
      (is (= 1 (kill-tasks :default)))
      (sleep 0.2)
      (is (= 1 (queue-count memo)))
      (is (typep (pop-queue memo) 'task-killed-error))
      (signals task-killed-error
        (call-ptree 'inf tree))
      (signals task-killed-error
        (call-ptree 'inf tree))
      (is (= 5 (call-ptree 'five tree))))))

(full-test clear-ptree-test
  (let ((tree (make-ptree))
        (count 0))
    (ptree-fn 'area '(width height) (lambda (w h) (* w h)) tree)
    (ptree-fn 'height '() (constantly 3) tree)
    (ptree-fn 'width '() (lambda () (incf count) 9) tree)
    (is (= 27 (call-ptree 'area tree)))
    (is (= 1 count))
    (is (= 27 (call-ptree 'area tree)))
    (is (= 1 count))
    (clear-ptree tree)
    (is (= 27 (call-ptree 'area tree)))
    (is (= 2 count))))

(full-test clear-ptree-errors-test
  (task-handler-bind ((foo-error #'invoke-transfer-error))
    (let ((tree (make-ptree))
          (count 0)
          (ready nil))
      (ptree-fn 'area '(width height) (lambda (w h) (* w h)) tree)
      (ptree-fn 'height '() (constantly 3) tree)
      (ptree-fn 'width '()
                (lambda ()
                  (incf count)
                  (if ready
                      9
                      (error 'foo-error)))
                tree)
      (signals foo-error
        (call-ptree 'area tree))
      (is (= 1 count))
      (signals foo-error
        (call-ptree 'area tree))
      (is (= 1 count))
      (clear-ptree-errors tree)
      (setf ready t)
      (is (= 27 (call-ptree 'area tree)))
      (is (= 2 count))
      (is (= 27 (call-ptree 'area tree)))
      (is (= 2 count)))))

(base-test ptree-multi-error-test
  (with-temp-kernel (2)
    (task-handler-bind ((foo-error #'invoke-transfer-error))
      (let ((timer-finish-p nil))
        (with-thread ()
          (sleep 0.25)
          (setf timer-finish-p t))
        (ptree ((area (width height) (* width height))
                (width () (sleep 0.5) 99)
                (height () (error 'foo-error)))
          (signals foo-error
            height)
          (is (not timer-finish-p))
          (signals foo-error
            area)
          (is (not timer-finish-p))
          (= 99 width)
          (is (not (null timer-finish-p)))))
      (let ((timer-finish-p nil))
        (with-thread ()
          (sleep 0.25)
          (setf timer-finish-p t))
        (ptree ((area (width height) (* width height))
                (width () (sleep 0.5) 99)
                (height () (error 'foo-error)))
          (signals foo-error
            area)
          (is (not timer-finish-p))
          (signals foo-error
            height)
          (is (not timer-finish-p))
          (= 99 width)
          (is (identity timer-finish-p)))))))

(full-test ptree-query-test
  (let ((tree (make-ptree)))
    (signals ptree-undefined-function-error
      (ptree-computed-p 'foo tree))
    (ptree-fn 'z '(x y) #'+ tree)
    (ptree-fn 'x () (lambda () 3) tree)
    (ptree-fn 'y () (lambda () 4) tree)
    (is (not (ptree-computed-p 'x tree)))
    (is (not (ptree-computed-p 'y tree)))
    (is (not (ptree-computed-p 'z tree)))
    (is (= 3 (call-ptree 'x tree)))
    (is (ptree-computed-p 'x tree))
    (is (not (ptree-computed-p 'y tree)))
    (is (not (ptree-computed-p 'z tree)))
    (is (= 7 (call-ptree 'z tree)))
    (is (ptree-computed-p 'x tree))
    (is (ptree-computed-p 'y tree))
    (is (ptree-computed-p 'z tree))))
