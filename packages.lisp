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

(defpackage #:lparallel.util
  (:documentation
   "(private) Miscellaneous utilities.")
  (:use #:cl)
  (:export #:with-gensyms
           #:defmacro/once
           #:unsplice
           #:symbolicate
           #:with-parsed-body)
  (:export #:while
           #:until
           #:repeat
           #:when-let
           #:dosequence
           #:alias-function
           #:alias-macro
           #:unwind-protect/ext
           #:import-now)
  (:export #:defun/inline
           #:defun/type
           #:defun/type/inline)
  (:export #:defslots
           #:defpair)
  (:export #:interact
           #:ensure-function
           #:to-boolean)
  (:export #:index)
  (:export #:*normal-optimize*
           #:*full-optimize*))

(defpackage #:lparallel.thread-util
  (:documentation
   "(private) Thread utilities.")
  (:use #:cl
        #:lparallel.util)
  (:export #:with-thread
           #:with-lock-predicate/wait
           #:with-lock-predicate/no-wait
           #:define-locking-fn
           #:define-simple-locking-fn
           #:condition-notify-and-yield)
  (:export #:make-lock
           #:make-condition-variable
           #:with-lock-held
           #:condition-wait
           #:destroy-thread
           #:current-thread)
  #+lparallel.with-green-threads
  (:export #:thread-yield))

(defpackage #:lparallel.raw-queue
  (:documentation
   "(private) Raw queue data structure.")
  (:use #:cl
        #:lparallel.util)
  (:export #:raw-queue
           #:make-raw-queue
           #:push-raw-queue
           #:pop-raw-queue
           #:peek-raw-queue
           #:raw-queue-count
           #:raw-queue-empty-p))

(defpackage #:lparallel.queue
  (:documentation
   "Blocking FIFO queue for communication between threads.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.raw-queue)
  (:export #:queue
           #:make-queue
           #:push-queue    #:push-queue/no-lock
           #:pop-queue     #:pop-queue/no-lock
           #:peek-queue    #:peek-queue/no-lock
           #:queue-count   #:queue-count/no-lock
           #:queue-empty-p #:queue-empty-p/no-lock
           #:try-pop-queue #:try-pop-queue/no-lock
           #:with-locked-queue))

(defpackage #:lparallel.biased-queue
  (:documentation
   "(private) Blocking two-tiered priority queue.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.raw-queue)
  (:export #:biased-queue
           #:make-biased-queue
           #:push-biased-queue     #:push-biased-queue/no-lock
           #:push-biased-queue/low #:push-biased-queue/low/no-lock
           #:pop-biased-queue      #:pop-biased-queue/no-lock
           #:peek-biased-queue     #:peek-biased-queue/no-lock
           #:biased-queue-empty-p  #:biased-queue-empty-p/no-lock
           #:try-pop-biased-queue  #:try-pop-biased-queue/no-lock
           #:pop-biased-queue      #:pop-biased-queue/no-lock
           #:biased-queue-count    #:biased-queue-count/no-lock
           #:with-locked-biased-queue))

(defpackage #:lparallel.counter
  (:documentation
   "(private) Atomic counter.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util)
  (:export #:counter
           #:make-counter
           #:inc-counter
           #:dec-counter
           #:counter-value))

(defpackage #:lparallel.spin-queue
  (:documentation
   "(private) Thread-safe FIFO queue which spins instead of locks.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.raw-queue
        #:lparallel.counter)
  (:export #:spin-queue
           #:make-spin-queue
           #:push-spin-queue
           #:pop-spin-queue
           #:peek-spin-queue
           #:spin-queue-count
           #:spin-queue-empty-p))

(defpackage #:lparallel.kernel
  (:documentation
   "Encompasses the scheduling and execution of parallel tasks using a
   pool of worker threads. All parallelism in lparallel is done on top
   of the kernel.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.queue
        #:lparallel.biased-queue
        #:lparallel.spin-queue
        #:lparallel.counter)
  (:export #:make-kernel
           #:check-kernel
           #:end-kernel
           #:kernel-worker-count
           #:kernel-bindings
           #:kernel-name
           #:kernel-context)
  (:export #:make-channel
           #:submit-task
           #:submit-timeout
           #:cancel-timeout
           #:receive-result
           #:try-receive-result
           #:do-fast-receives
           #:kill-tasks
           #:task-handler-bind
           #:task-categories-running
           #:invoke-transfer-error)
  (:export #:*kernel*
           #:*kernel-spin-count*
           #:*task-category*
           #:*task-priority*
           #:*debug-tasks-p*)
  (:export #:transfer-error
           #:no-kernel-error
           #:kernel-creation-error
           #:task-killed-error))

(defpackage #:lparallel.kernel-util
  (:documentation
   "(semi-private) Abstracts some common patterns for submitting and
   receiving tasks. This probably won't change, but no guarantees.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.kernel
        #:lparallel.queue
        #:lparallel.counter)
  (:export #:with-submit-counted
           #:submit-counted
           #:receive-counted)
  (:export #:with-submit-indexed
           #:submit-indexed
           #:receive-indexed)
  (:export #:with-submit-cancelable
           #:submit-cancelable
           #:receive-cancelables))

(defpackage #:lparallel.ptree
  (:documentation
   "A ptree is a computation represented by a tree together with
   functionality to execute the tree in parallel.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.kernel
        #:lparallel.queue)
  (:export #:ptree
           #:ptree-fn
           #:make-ptree
           #:check-ptree
           #:call-ptree
           #:ptree-computed-p
           #:clear-ptree
           #:clear-ptree-errors
           #:*ptree-node-kernel*)
  (:export #:ptree-undefined-function-error
           #:ptree-lambda-list-keyword-error
           #:ptree-redefinition-error))

(defpackage #:lparallel.promise
  (:documentation
   "Promises and futures.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.kernel)
  (:export #:promise
           #:future
           #:speculate
           #:delay
           #:force
           #:fulfill
           #:fulfilledp
           #:chain))

(defpackage #:lparallel.defpun
  (:documentation "Fine-grained parallelism.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.kernel
        #:lparallel.thread-util)
  (:export #:defpun
           #:defpun*
           #:defpun/type
           #:defpun/type*
           #:declaim-defpun
           #:plet
           #:plet-if))

(defpackage #:lparallel.cognate
  (:documentation
   "Parallelized versions of some Common Lisp functions.")
  (:use #:cl
        #:lparallel.util
        #:lparallel.kernel
        #:lparallel.kernel-util
        #:lparallel.promise
        #:lparallel.defpun)
  (:export #:pand
           #:pcount
           #:pcount-if
           #:pcount-if-not
           #:pdotimes
           #:pevery
           #:pfind
           #:pfind-if
           #:pfind-if-not
           #:pfuncall
           #:plet
           #:plet-if
           #:pmap
           #:pmapc
           #:pmapcan
           #:pmapcar
           #:pmapcon
           #:pmap-into
           #:pmapl
           #:pmaplist
           #:pmaplist-into
           #:pmap-reduce
           #:pnotany
           #:pnotevery
           #:por
           #:preduce
           #:preduce-partial
           #:premove
           #:premove-if
           #:premove-if-not
           #:psome
           #:psort
           #:psort*))

(defpackage #:lparallel
  (:use #:cl))

(dolist (package '(#:lparallel.kernel
                   #:lparallel.promise
                   #:lparallel.defpun
                   #:lparallel.cognate
                   #:lparallel.ptree))
  (use-package package (find-package '#:lparallel))
  (export
   (loop :for symbol :being :the :external-symbols :in package :collect symbol)
   (find-package '#:lparallel)))

(setf (documentation (find-package '#:lparallel) t)
      "This is a convenience package which exports the external symbols of:
   lparallel.kernel
   lparallel.promise
   lparallel.defpun
   lparallel.cognate
   lparallel.ptree")
