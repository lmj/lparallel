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

(in-package #:lparallel-bench)

(defparameter *trials* 12)

(defparameter *rehearsals* 8)

(defparameter *repeat-gc* #-abcl 20
                          ;; (gc) hangs on abcl
                          #+abcl  0)

(defparameter *benches* '(bench-pmap
                          bench-psort
                          bench-preduce
                          bench-fib
                          bench-mm))

(defparameter *sizes* '(10 100 500 1000 5000 10000 50000 100000 200000))

(defparameter *fib-n* '(5 10 15 20 25 30 35))

(defparameter *mm-n* '(10 50 100 200))

(defun desc (size op fn time)
  (format nil "~&size ~6d | op ~8,a | ~10,a ~10,d~%" size op fn time))

(defun desc-n (n fn time)
  (format nil "~&n ~6d | ~15,a ~8,d~%" n fn time))

(defmacro with-fns (fns &body body)
  `(let ,(loop :for fn :in fns :collect `(,fn (symbol-function ,fn)))
     ,@body))

(defun reset ()
  (sleep 0.2)
  (repeat (* 3 *repeat-gc*) (gc)))

(defun make-cleanup ()
  (make-bench-spec
   :args-fn (lambda () nil)
   :exec-fn (lambda () (repeat *repeat-gc* (gc)))
   :desc-fn (lambda (&rest args) (declare (ignore args)))))

(defmacro/once collect-trials (&once trials &body body)
  `(progn
     (assert (>= ,trials 2))
     (collect (make-cleanup))
     (repeat (- ,trials 1)
       (collect ,@body))))

(defun bench-pmap ()
  (let ((fns        '(map pmap))
        (trials     *trials*)
        (rehearsals *rehearsals*))
    (bench
     (length fns)
     trials
     rehearsals
     (collecting1
      (dolist (fn fns)
        (dolist (size *sizes*)
          (let1 source (make-random-vector size)
            (dolist (op '(sin))
              (rebind (fn size op)
                (collect-trials trials
                  (make-bench-spec
                   :args-fn (lambda ()
                              (list source))
                   :exec-fn (with-fns (op)
                              (lambda (source)
                                (funcall fn 'vector op source)))
                   :desc-fn (lambda (time)
                              (desc size op fn time)))))))))))))

(defun bench-psort ()
  (let ((fns        '(sort psort))
        (trials     *trials*)
        (rehearsals *rehearsals*))
    (bench
     (length fns)
     trials
     rehearsals
     (collecting1
      (dolist (fn fns)
        (dolist (size *sizes*)
          (let1 source (make-random-vector size)
            (dolist (op '(<))
              (rebind (fn size op)
                (collect-trials trials
                  (make-bench-spec
                   :args-fn (lambda ()
                              (list (copy-seq source)))
                   :exec-fn (with-fns (op)
                              (lambda (source)
                                (funcall fn source op)))
                   :desc-fn (lambda (time)
                              (desc size op fn time)))))))))))))

(defun bench-preduce ()
  ;; reduce needs more CPU unthrottling
  (let* ((fns        '(reduce preduce))
         (unthrottle 10)
         (trials     (+ unthrottle *trials*))
         (rehearsals (+ unthrottle *rehearsals*)))
    (bench
     (length fns) 
     trials
     rehearsals
     (collecting1
      (dolist (fn fns)
        (dolist (size *sizes*)
          (let1 source (make-random-vector size)
            (dolist (op '(+))
              (rebind (fn size op)
                (collect-trials trials
                  (make-bench-spec
                   :args-fn (lambda ()
                              (list source))
                   :exec-fn (with-fns (op)
                              (lambda (source)
                                (funcall fn op source)))
                   :desc-fn (lambda (time)
                              (desc size op fn time)))))))))))))

(defun fib-let (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2)
      n
      (let ((a (fib-let (- n 1)))
            (b (fib-let (- n 2))))
        (+ a b))))

(defpar fib-plet (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2)
      n
      (plet ((a (fib-plet (- n 1)))
             (b (fib-plet (- n 2))))
        (+ a b))))

(defpar fib-plet-if (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2)
      n
      (plet-if (> n 20)
          ((a (fib-plet-if (- n 1)))
           (b (fib-plet-if (- n 2))))
        (+ a b))))

(defun bench-fib ()
  (let ((fns        '(fib-let fib-plet fib-plet-if))
        (trials     *trials*)
        (rehearsals *rehearsals*))
    (bench
     (length fns)
     trials
     rehearsals
     (collecting1
      (dolist (fn fns)
        (dolist (n *fib-n*)
          (rebind (fn n)
            (collect-trials trials
              (make-bench-spec
               :args-fn (lambda ()
                          (list n))
               :exec-fn (lambda (n)
                          (funcall fn n))
               :desc-fn (lambda (time)
                          (desc-n n fn time)))))))))))

;;; mm (matrix mulitply) adapted from Vladimir Sedach's eager-future2,
;;; which in turn credits the following:
;;; 
;;; benchmarks from Appendix A of Marc Feeley's PhD dissertation:
;;; Marc Feeley. An Efficient and General Implementation of Futures on
;;; Large Scale Shared-Memory Multiprocessors. PhD thesis, Brandeis
;;; University, April 1993.
;;; http://www.iro.umontreal.ca/~feeley/papers/FeeleyPhD.pdf

(defmacro define-mm (name def xlet)
  `(,def ,name (n m1 m2 m3)               ; m1 * m2 -> m3
     (declare (optimize (speed 3)))
     (labels
         ((compute-entry (row col)     ; loop to compute inner product
            (labels ((compute-loop (i j sum)
                       (if (>= j 0)
                           (compute-loop (- i 1)
                                         (- j n)
                                         (+ sum (* (aref m1 i) (aref m2 j))))
                           (setf (aref m3 (+ i 1 col)) sum))))
              (compute-loop (+ row n -1) (+ (* n (1- n)) col) 0)))

          (compute-cols-between (row i j) ; DAC over columns
            (if (= i j)
                (compute-entry row i)
                (let1 mid (floor (+ i j) 2)
                  (,xlet ((half1 (compute-cols-between row i mid)))
                    (compute-cols-between row (+ mid 1) j)
                    half1))))

          (compute-rows-between (i j)   ; DAC over rows
            (if (= i j)
                (compute-cols-between (* i n) 0 (- n 1))
                (let1 mid (floor (+ i j) 2)
                  (,xlet ((half1 (compute-rows-between i mid)))
                    (compute-rows-between (+ mid 1) j)
                    half1)))))

       (compute-rows-between 0 (1- n)))))

(define-mm mm defun let)
(define-mm pmm defpar plet)

(defun run-mm (fn n)
  (funcall fn
           n
           (make-array (* n n) :initial-element 2)
           (make-array (* n n) :initial-element 2)
           (make-array (* n n) :initial-element nil)))

(defun bench-mm ()
  (let ((fns        '(mm pmm))
        (trials     *trials*)
        (rehearsals *rehearsals*))
    (bench
     (length fns)
     trials
     rehearsals
     (collecting1
      (dolist (fn fns)
        (dolist (n *mm-n*)
          (rebind (fn n)
            (collect-trials trials
              (make-bench-spec
               :args-fn (lambda ()
                          (list n))
               :exec-fn (lambda (n)
                          (run-mm fn n))
               :desc-fn (lambda (time)
                          (desc-n n fn time)))))))))))

(defun execute (num-workers)
  (format t "~%")
  (when (find :swank *features*)
    (format t "* Benchmarking with SLIME may produce inaccurate results!~%~%"))
  (format t "* Have you unthrottled your CPUs? See bench/README.~%~%")
  (format t "Running benchmarks with ~a workers.~%~%" num-workers)
  (apply #'run-suite num-workers #'reset *benches*))
