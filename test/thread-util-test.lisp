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

(base-test basic-threading-test
  (let ((num-threads 10)
        (num-objects 1000)
        (num-iterations 5)
        (from-workers (make-queue))
        (to-workers (make-queue)))
    (repeat num-threads
      (with-thread ()
        (loop (let ((object (pop-queue to-workers)))
                (if object
                    (push-queue object from-workers)
                    (return))))))
    (repeat num-iterations
      (repeat num-objects
        (push-queue 99 to-workers))
      (repeat num-objects
        (pop-queue from-workers)))
    (repeat num-threads
      (push-queue nil to-workers))
    (sleep 0.5)
    (is (= 0 (queue-count from-workers)))
    (is (= 0 (queue-count to-workers)))))

(base-test thread-bindings-test
  (setf *memo* :main)
  (with-thread ()
    (setf *memo* :child))
  (sleep 0.2)
  (is (eq :child *memo*))

  (setf *memo* :main)
  (with-thread (:bindings (list (cons '*memo* *memo*)))
    (setf *memo* :child))
  (sleep 0.2)
  (is (eq :main *memo*)))

#-lparallel.without-kill
(base-test destroy-thread-cleanup-test
  (let* ((cleanedp nil)
         (thread (with-thread ()
                   (unwind-protect (sleep 999999)
                     (setf cleanedp t)))))
    (sleep 0.2)
    (destroy-thread thread)
    (sleep 0.2)
    (is (eq t cleanedp))))
