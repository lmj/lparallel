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

;;;; util

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun home-symbols (pkg)
    (loop for sym being the present-symbols in pkg
          when (eq (find-package pkg) (symbol-package sym))
          collect sym))

  (defun home-functions (pkg)
    (remove-if-not #'fboundp (home-symbols pkg)))

  (defun packages-passing (predicate)
    (remove-if-not predicate (list-all-packages)))

  (defun home-functions-in-packages-passing (predicate)
    (reduce #'nconc (packages-passing predicate) :key #'home-functions))

  (defun match-package-p (string pkg)
    (search string (package-name pkg) :test #'equalp)))

(defmacro without-warnings (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))

;;;; profile

(defmacro profile-fns (syms)
  `(progn
     ,@(loop for sym in syms
             collect `(sb-profile:profile ,sym))))

(defun enable-profiling ()
  (profile-fns #.(home-functions-in-packages-passing
                  (lambda (pkg)
                    (or (match-package-p "lparallel" pkg)
                        (match-package-p "bordeaux-threads" pkg)
                        #+(and sbcl lparallel.with-stealing-scheduler)
                        (match-package-p "sb-concurrency" pkg))))))

(defun profile (&rest args)
  (without-warnings
    (enable-profiling))
  (sb-profile:reset)
  (apply #'execute args)
  (sb-profile:report))

;;;; stat-profile

(defun stat-profile (&rest args)
  (sb-sprof:with-profiling (:max-samples 100000
                            :sample-interval (/ sb-sprof:*sample-interval* 2)
                            :report :graph
                            :loop nil
                            :threads :all
                            :show-progress nil)
    (apply #'execute args)))
