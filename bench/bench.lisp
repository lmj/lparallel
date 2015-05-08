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

;;;; helpers

(defmacro collecting1 (&body body)
  (with-gensyms (result value)
    `(let ((,result nil))
       (flet ((collect (,value) (push ,value ,result)))
         ,@body)
       (nreverse ,result))))

(defun groups-of (n list)
  (loop for pos on list by (partial-apply 'nthcdr n)
        collect (subseq pos 0 n)))

(defun riffle (groups deck)
  (apply #'mapcar #'list (groups-of (/ (length deck) groups) deck)))

;;;; wall time

#-sbcl
(progn
  (alias-function get-time get-internal-real-time)
  (defun time-interval (start end)
    (- end start)))

#+sbcl
(progn
  (defun get-time ()
    (multiple-value-list (sb-ext:get-time-of-day)))
  (defun to-microseconds (time)
    (destructuring-bind (sec usec) time
      (+ (* 1000000 sec) usec)))
  (defun time-interval (start end)
    (- (to-microseconds end)
       (to-microseconds start))))

(defun wall-time (fn args)
  (let ((start (get-time)))
    (apply fn args)
    (let ((end (get-time)))
      (time-interval start end))))

;;;; bench

(defslots bench-spec ()
  ((args-fn :reader args-fn)
   (exec-fn :reader exec-fn)
   (desc-fn :reader desc-fn))
  (:documentation
   "A benchmark specification.

`args-fn' creates the arguments to be passed to `exec-fn'. The
execution time of `exec-fn' is passed to `desc-fn', which returns a
descriptive string."))

(alias-function make-bench-spec make-bench-spec-instance)

(defun print-chunk (chunk)
  (format t "~&")
  (mapc 'princ chunk)
  (format t "~%"))

(defun ping (x)
  (format t ".")
  x)

(defun bench (num-fns num-trials num-rehearsals specs)
  "Run bench specs.

To minimize GC interactions, all arguments are generated at the outset
and each benchmarked function is held constant while the generated
argument lists are applied successively.

When benchmarks are complete, the rehearsals are discarded and the
results are riffled for comparison."
  (mapc 'print-chunk
   (mapcar 'flatten
    (riffle num-fns
     (mapcar (partial-apply 'nthcdr num-rehearsals)
      (groups-of num-trials
       (mapcar 'funcall
               (mapcar 'desc-fn specs)
               (mapcar (compose 'ping 'wall-time)
                       (mapcar 'exec-fn specs)
                       (mapcar (compose 'funcall 'args-fn) specs)))))))))
