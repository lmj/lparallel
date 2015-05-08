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

(defparameter *repeat-gc* #-abcl 50
                          ;; (gc) hangs on abcl
                          #+abcl  0)

(defparameter *benches*
  `((bench-pmap
     (map pmap)
     (10 50 100 500 1000 5000 10000 50000 100000 500000)
     ,*trials*
     ,*rehearsals*)

    (bench-preduce
     (reduce preduce)
     (10 50 100 500 1000 5000 10000 50000 100000 500000)
     ;; needs more unthrottling
     ,(+ 10 *trials*)
     ,(+ 10 *rehearsals*))

    (bench-psort
     (sort psort)
     (10 50 100 500 1000 5000 10000 50000 100000 200000)
     ,*trials*
     ,*rehearsals*)

    (bench-pfib
     (fib-let fib-plet fib-plet-if)
     (5 10 15 20 25 30 35)
     ,*trials*
     ,*rehearsals*)

    (bench-pmatrix-mul
     (matrix-mul pmatrix-mul)
     (5 10 50 100 200)
     ,*trials*
     ,*rehearsals*)))

(defparameter *use-caller* '(bench-pfib bench-pmatrix-mul bench-psort))

(defparameter *spin-count* 20000)

(defun data (name)
  (rest (find name *benches* :key #'first)))

(defun desc-size-op (size op fn time)
  (format nil "~&size ~6d | op ~8,a | ~10,a ~10,d~%" size op fn time))

(defun desc-n (n fn time)
  (format nil "~&n ~6d | ~15,a ~8,d~%" n fn time))

(defmacro with-fns (fns &body body)
  `(let ,(loop for fn in fns
               collect `(,fn (symbol-function ,fn)))
     ,@body))

(defun reset ()
  (sleep 0.2)
  (repeat *repeat-gc*
    (gc :full t)))

(defmacro collect-trials (trials &body body)
  `(repeat ,trials
     (collect ,@body)))

(defmacro defbench (name params &body body)
  `(defun ,name ()
     (destructuring-bind ,params (data ',name)
       ,@body)))

(defmacro rebind (vars &body body)
  `(let ,(mapcar #'list vars vars)
     ,@body))

(defun make-random-vector (size)
  (map-into (make-array size :element-type 'single-float)
            (lambda () (random 1.0f0))))

(defbench bench-pmap (fns inputs trials rehearsals)
  (bench
   (length fns)
   trials
   rehearsals
   (collecting1
     (dolist (fn fns)
       (dolist (size inputs)
         (let ((source (make-random-vector size)))
           (dolist (op '(sin))
             (rebind (fn size op)
               (collect-trials trials
                 (make-bench-spec
                  :args-fn (lambda ()
                             (list source))
                  :exec-fn (with-fns (op)
                             (lambda (source)
                               (funcall
                                fn `(vector single-float ,size) op source)))
                  :desc-fn (lambda (time)
                             (desc-size-op size op fn time))))))))))))

(defbench bench-psort (fns inputs trials rehearsals)
  (bench
   (length fns)
   trials
   rehearsals
   (collecting1
     (dolist (fn fns)
       (dolist (size inputs)
         (let ((source (make-random-vector size)))
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
                             (desc-size-op size op fn time))))))))))))

(defbench bench-preduce (fns inputs trials rehearsals)
  (bench
   (length fns)
   trials
   rehearsals
   (collecting1
     (dolist (fn fns)
       (dolist (size inputs)
         (let ((source (make-random-vector size)))
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
                             (desc-size-op size op fn time))))))))))))

(defun fib-let (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2)
      n
      (let ((a (fib-let (- n 1)))
            (b (fib-let (- n 2))))
        (+ a b))))

(defpun fib-plet (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2)
      n
      (plet ((a (fib-plet (- n 1)))
             (b (fib-plet (- n 2))))
        (+ a b))))

(defpun fib-plet-if (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2)
      n
      (plet-if (> n 15)
          ((a (fib-plet-if (- n 1)))
           (b (fib-plet-if (- n 2))))
        (+ a b))))

(defbench bench-pfib (fns inputs trials rehearsals)
  (bench
   (length fns)
   trials
   rehearsals
   (collecting1
     (dolist (fn fns)
       (dolist (n inputs)
         (rebind (fn n)
           (collect-trials trials
             (make-bench-spec
              :args-fn (lambda ()
                         (list n))
              :exec-fn (lambda (n)
                         (funcall fn n))
              :desc-fn (lambda (time)
                         (desc-n n fn time))))))))))

;;; mm (matrix multiply) adapted from Vladimir Sedach's eager-future2,
;;; which in turn credits the following:
;;;
;;; benchmarks from Appendix A of Marc Feeley's PhD dissertation:
;;; Marc Feeley. An Efficient and General Implementation of Futures on
;;; Large Scale Shared-Memory Multiprocessors. PhD thesis, Brandeis
;;; University, April 1993.
;;; http://www.iro.umontreal.ca/~feeley/papers/FeeleyPhD.pdf

(defmacro define-mm (name def xlet)
  `(,def ,name (n m1 m2 m3)               ; m1 * m2 -> m3
     (declare (optimize (speed 3) (debug 0) (safety 0)))
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
                (let ((mid (floor (+ i j) 2)))
                  (,xlet ((half1 (compute-cols-between row i mid))
                          (half2 (compute-cols-between row (+ mid 1) j)))
                    half1
                    half2))))

          (compute-rows-between (i j)   ; DAC over rows
            (if (= i j)
                (compute-cols-between (* i n) 0 (- n 1))
                (let ((mid (floor (+ i j) 2)))
                  (,xlet ((half1 (compute-rows-between i mid))
                          (half2 (compute-rows-between (+ mid 1) j)))
                    half1
                    half2)))))

       (compute-rows-between 0 (1- n)))))

(define-mm matrix-mul defun let)
(define-mm pmatrix-mul defpun plet)

(defun run-mm (fn n)
  (funcall fn
           n
           (make-array (* n n) :initial-element 2)
           (make-array (* n n) :initial-element 2)
           (make-array (* n n) :initial-element nil)))

(defbench bench-pmatrix-mul (fns inputs trials rehearsals)
  (bench
   (length fns)
   trials
   rehearsals
   (collecting1
     (dolist (fn fns)
       (dolist (n inputs)
         (rebind (fn n)
           (collect-trials trials
             (make-bench-spec
              :args-fn (lambda ()
                         (list n))
              :exec-fn (lambda (n)
                         (run-mm fn n))
              :desc-fn (lambda (time)
                         (desc-n n fn time))))))))))

(defun select-benches (fn-names)
  (mapcar (lambda (name)
            (assoc (intern (symbol-name name) :lparallel-bench)
                   *benches*))
          fn-names))

(defun call-with-temp-kernel (worker-count use-caller fn)
  (with-temp-kernel (worker-count
                     :spin-count *spin-count*
                     :use-caller use-caller)
    (funcall fn)))

(defvar *last-random-state* nil)

(defun execute (num-workers &rest fns)
  (format t "~%")
  (when (find :swank *features*)
    (format t "* Benchmarking with SLIME may produce inaccurate results!~%~%"))
  (format t "* Have you unthrottled your CPUs? See bench/README.~%~%")
  (format t "Running benchmarks with ~a workers.~%~%" num-workers)
  (let ((*random-state* (make-random-state t)))
    (setf *last-random-state* (make-random-state *random-state*))
    (dolist (spec (if fns (select-benches fns) *benches*))
      (let ((fn (first spec)))
        (if (member fn *use-caller*)
            (call-with-temp-kernel (- num-workers 1) t fn)
            (call-with-temp-kernel num-workers nil fn))
        (reset)))))
