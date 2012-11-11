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

;;; default to stealing scheduler on sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (find :sbcl *features*)
             (not (find :lparallel.without-stealing-scheduler *features*)))
    (pushnew :lparallel.with-stealing-scheduler *features*))

  (when (and (find :allegro *features*)
             (not (find :os-threads *features*)))
    (pushnew :lparallel.with-green-threads *features*)))

(defsystem :lparallel
  :version "2.0.2"
  :description "Parallelism for Common Lisp"
  :long-description
"
lparallel is a library for parallel programming in Common Lisp, featuring

  * a simple model of task submission with receiving queue
  * fine-grained parallelism
  * asynchronous condition handling across thread boundaries
  * parallel versions of map, reduce, sort, remove, and many others
  * promises, futures, and delayed evaluation constructs
  * computation trees for parallelizing interconnected tasks
  * high and low priority tasks
  * task killing by category
  * integrated timeouts
  * vector-based FIFO queues

See http://lparallel.org for documentation and examples.
"
  :licence "BSD"
  :author "James M. Lawrence <llmjjmll@gmail.com>"
  :depends-on (:bordeaux-threads)
  :serial t
  :components ((:file "packages")
               (:module "src"
                :serial t
                :components ((:module "util"
                              :serial t
                              :components ((:file "config")
                                           (:file "misc")
                                           (:file "defmacro")
                                           (:file "defun")
                                           (:file "defslots")
                                           (:file "defpair")))
                             (:file "thread-util")
                             (:file "raw-queue")
                             (:file "queue")
                             (:file "counter")
                             (:file "biased-queue")
                             (:file "spin-queue")
                             (:module "kernel"
                              :serial t
                              :components ((:file "specials")
                                           (:file "handling")
                                           (:file "classes")
       #-lparallel.with-stealing-scheduler (:file "central-scheduler")
       #+lparallel.with-stealing-scheduler (:file "stealing-scheduler")
                                    #-abcl (:file "kill")
                                           (:file "core")
                                           (:file "timeout")))
                             (:file "kernel-util")
                             (:file "promise")
                             (:file "ptree")
                             (:file "defpun")
                             (:module "cognate"
                              :serial t
                              :components ((:file "util")
                                           (:file "option")
                                           (:file "subdivide")
                                           (:file "pandor")
                                           (:file "plet")
                                           (:file "pmap")
                                    #-abcl (:file "pmap-open-coded")
                                           (:file "pdotimes")
                                           (:file "pqualifier")
                                           (:file "preduce")
                                           (:file "premove")
                                           (:file "pfind")
                                           (:file "pcount")
                                           (:file "psort")))))))

(defmethod perform ((o test-op) (c (eql (find-system :lparallel))))
  (declare (ignore o c))
  (load-system '#:lparallel-test)
  (test-system '#:lparallel-test))

(defmethod perform :after ((o load-op) (c (eql (find-system :lparallel))))
  (declare (ignore o c))
  (pushnew :lparallel *features*))
