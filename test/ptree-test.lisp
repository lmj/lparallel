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

(lp-test basic-ptree-test
  (ptree ((a (b c) (- b c))
          (b ()    3)
          (c ()    4))
    (is (= a (- 3 4)))
    (is (= b 3))
    (is (= c 4))))

(lp-test basic-ptree-2-test
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

(lp-test basic-ptree-fn-test
  (let1 tree (make-ptree)
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

(lp-test no-double-compute-test
  (let1 tree (make-ptree)
    (setf *memo* 0)
    (ptree-fn 'f '(x) (lambda (x) (* x x)) tree)
    (ptree-fn 'x '()  (lambda () (incf *memo*) 5) tree)
    (is (= 25 (call-ptree 'f tree)))
    (is (= 1 *memo*))
    (is (= 25 (call-ptree 'f tree)))
    (is (= 1 *memo*))
    (is (= 5 (call-ptree 'x tree)))
    (is (= 1 *memo*))))

(lp-test lone-fn-test
  (ptree ((lone () 7))
    (is (= 7 lone))))

(lp-test missing-node-test
  (let1 tree (make-ptree)
    (ptree-fn 'f '(x) (lambda (x) (* x x)) tree)
    (signals ptree-undefined-function-error
      (call-ptree 'g tree))))

(lp-test missing-ptree-function-test
  (let1 tree (make-ptree)
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
                     (sort (copy-list refs) #'string<))))))))

(lp-test redefinition-test
  (signals ptree-redefinition-error
    (let1 tree (make-ptree)
      (ptree-fn 'foo () (lambda () 3) tree)
      (ptree-fn 'foo () (lambda () 4) tree))))

(lp-test lambda-list-keywords-in-ptree-test
  (signals ptree-lambda-list-keyword-error
    (eval '(ptree ((foo (x &optional y) (list x y))))))
  (signals ptree-lambda-list-keyword-error
    (eval '(ptree ((foo (x &REST y) (list x y)))))))

(lp-test error-inside-ptree-function-test
  (let1 tree (make-ptree)
    (ptree-fn 'root '(child) (lambda (x) x) tree)
    (ptree-fn 'child () (lambda () (error 'foo-error)) tree)
    (let1 err nil
      (handler-case (call-ptree 'root tree)
        (error (result) (setf err result)))
      (is (not (null err)))
      (is (eq 'foo-error (type-of err))))))

(defmacro/once for-range ((var &once pair) &body body)
  `(loop
      :for ,var :from (first ,pair) :to (second ,pair)
      :do (progn ,@body)))

(lp-test grind-ptree-test
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
            (let1 tree (make-ptree)
              (setf count 0)
              (generate-ptree num-levels num-children tree)
              (is (eql count (call-ptree root tree))))))))))

(lp-base-test ptree-node-kernel-test
  (let1 *ptree-node-kernel* (make-kernel 2)
    (unwind-protect 
         (with-new-kernel (1)
           (is (equal
                '(63 9 7 1)
                (ptree ((area   (width height) (* width height))
                        (width  (border)       (+ 7 (* 2 border)))
                        (height (border)       (+ 5 (* 2 border)))
                        (border ()             (let1 channel (make-channel)
                                                 (submit-task
                                                  channel (lambda () 1))
                                                 (receive-result channel))))
                  ;; will hang without separate node kernel
                  (list area width height border)))))
      (let1 *kernel* *ptree-node-kernel*
        (end-kernel)))))

(lp-test ptree-node-id-test
  (let ((tree   (make-ptree))
        (area   (cons nil nil))
        (width  (cons nil nil))
        (height 'height)
        (border (cons nil nil)))
    (ptree-fn area   (list width height) (lambda (w h) (* w h))       tree)
    (ptree-fn width  (list border)       (lambda (b)   (+ 7 (* 2 b))) tree)
    (ptree-fn height (list border)       (lambda (b)   (+ 5 (* 2 b))) tree)
    (ptree-fn border '()                 (lambda ()    1)             tree)
    (is (= 63 (call-ptree area tree)))))
