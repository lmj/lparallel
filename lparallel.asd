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

(defsystem :lparallel
  :version "1.2.2"
  :description "Parallelism for Common Lisp"
  :long-description
"
lparallel provides low-level and high-level tools for writing
efficient parallel programs in Common Lisp.

The low-level API of lparallel is meant to describe parallelism in a
generic manner. The current implementation of the API uses a group of
worker threads, though in principle other implementations are
possible (for instance an interface to a distributed system).

lparallel also provides higher-level facilities including:

* asynchronous condition handling across thread boundaries
* parallel versions of map, reduce, sort, remove, and many others
* promises, futures, and delayed evaluation constructs
* computation trees for parallelizing interconnected tasks
* high and low priority tasks
* task killing by category
* integrated timeouts
* vector-based FIFO queues

See http://lparallel.com for documentation and examples.
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
                                           (:file "macro-writing")
                                           (:file "misc")
                                           (:file "defmacro")
                                           (:file "defun")
                                           (:file "defslots")))
                             (:file "thread-util")
                             (:file "raw-queue")
                             (:file "queue")
                             (:file "counter")
                             (:file "biased-queue")
                             (:module "kernel"
                              :serial t
                              :components ((:file "util")
                                           (:file "thread-locals")
                                           (:file "handling")
                                           (:file "classes")
                                           (:file "simple-scheduler")
                                           (:file "core")
                                           (:file "timeout")))
                             (:file "kernel-util")
                             (:file "promise")
                             (:file "ptree")
                             (:module "cognate"
                              :serial t
                              :components ((:file "util")
                                           (:file "option")
                                           (:file "subdivide")
                                           (:file "pandor")
                                           (:file "plet")
                                           (:file "pmap")
                                           (:file "pqualifier")
                                           (:file "preduce")
                                           (:file "premove")
                                           (:file "psort")))))))

(defmethod perform ((o test-op) (c (eql (find-system :lparallel))))
  (declare (ignore o c))
  (load-system '#:lparallel-test)
  (test-system '#:lparallel-test))
