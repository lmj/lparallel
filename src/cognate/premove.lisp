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

(in-package #:lparallel.cognate)

(defun premove-if-not/list (test list &rest args &key start end key parts)
  (declare (ignore start end key parts))
  (declare (dynamic-extent args))
  (declare #.*full-optimize*)
  (reduce #'nreconc
          (apply #'preduce/partial
                 (lambda (acc x)
                   (if (funcall test x)
                       (cons x acc)
                       acc))
                 list
                 :initial-value nil
                 args)
          :initial-value nil
          :from-end t))

(defun premove-if-not (test sequence &rest args &key start end key parts)
  "Parallel version of `remove-if-not'. Note the `count' and
`from-end' options are not supported."
  (declare (ignore start end key parts))
  (declare (dynamic-extent args))
  (typecase sequence
    (list (apply #'premove-if-not/list test sequence args))
    (t    (apply #'remove-if-not test sequence
                 (plist-extract '(:start :end :key) args))))) 

(defun premove-if (test sequence &rest args &key start end key parts)
  "Parallel version of `remove-if'. Note the `count' and `from-end'
options are not supported."
  (declare (ignore start end key parts))
  (declare (dynamic-extent args))
  (typecase sequence
    (list (apply #'premove-if-not/list (complement test) sequence args))
    (t    (apply #'remove-if test sequence
                 (plist-extract '(:start :end :key) args)))))

(defun premove (item sequence
                &rest args
                &key test test-not start end key parts)
  "Parallel version of `remove'. Note the `count' and `from-end'
options are not supported."
  (declare (ignore start end key parts))
  (declare (dynamic-extent args))
  (typecase sequence
    (list
     (when (and test test-not)
       (error "Both `:test' and `:test-not' options given to `premove'."))
     (when test-not
       (setf test (complement test-not)
             test-not nil))
     (let1 unary-test (if test
                          (lambda (x) (funcall test item x))
                          (typecase item
                            ((or number character)
                             (lambda (x) (eql item x)))
                            (t
                             (lambda (x) (eq item x)))))
       (apply #'premove-if
              unary-test
              sequence
              (plist-extract '(:start :end :key :parts) args))))
    (t
     (apply #'remove item sequence
            (plist-extract '(:test :test-not :start :end :key) args)))))
