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

(in-package #:lparallel-bench)

(defparameter *trials* 12)

(defparameter *rehearsals* 8)

(defparameter *repeat-gc* #-abcl 20
                          ;; (gc) hangs on abcl
                          #+abcl  0)

(defparameter *benches* '(bench-pmap
                          bench-psort
                          bench-preduce))

(defparameter *sizes* '(10 100 500 1000 5000 10000 50000 100000 200000))

(defun desc (size op fn time)
  (format nil "~&size ~6d | op ~8,a | ~10,a ~10,d~%" size op fn time))

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

(defun execute (num-workers)
  (apply #'run-suite num-workers #'reset *benches*))
