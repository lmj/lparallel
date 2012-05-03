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
  (:use #:cl)
  (:export #:with-gensyms
           #:defmacro/once
           #:mklist
           #:unsplice
           #:intern-conc
           #:with-parsed-body)
  (:export #:while
           #:repeat
           #:when-let
           #:while-let
           #:dosequence
           #:rebind
           #:let1
           #:alias-function
           #:unwind-protect/ext)
  (:export #:defun/inline
           #:defun/type
           #:defun/type/inline)
  (:export #:defslots
           #:defpair)
  (:export #:interact
           #:ensure-function
           #:to-boolean)
  (:export #:*normal-optimize*
           #:*full-optimize*))

(defpackage #:lparallel.thread-util
  (:use #:cl
        #:lparallel.util
        #:bordeaux-threads)
  (:export #:with-thread
           #:with-lock-predicate/wait
           #:with-lock-predicate/no-wait
           #:define-thread-locals
           #:define-locking-fn
           #:define-simple-locking-fn
           #:condition-notify-and-yield)
  ;; exported from bordeaux-threads
  (:export #:make-thread
           #:make-lock
           #:make-condition-variable
           #:with-lock-held
           #:acquire-lock
           #:release-lock
           #:condition-wait))

(defpackage #:lparallel.raw-queue
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
           #:with-locked-biased-queue))

(defpackage #:lparallel.counter
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util)
  (:export #:counter
           #:make-counter
           #:inc-counter
           #:dec-counter
           #:counter-value))

(defpackage #:lparallel.spin-queue
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
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.queue
        #:lparallel.biased-queue
        #:lparallel.spin-queue)
  (:export #:make-kernel
           #:kernel-worker-count
           #:check-kernel
           #:end-kernel
           #:kernel-special-bindings)
  (:export #:make-channel
           #:submit-task
           #:submit-timeout
           #:cancel-timeout
           #:receive-result
           #:do-fast-receives
           #:kill-tasks
           #:task-handler-bind)
  (:export #:*kernel*
           #:*kernel-spin-count*
           #:*task-category*
           #:*task-priority*)
  (:export #:transfer-error
           #:no-kernel-error
           #:task-killed-error)
  (:export #:*kernel-task-category* ; deprecated; same as *task-category*
           #:*kernel-task-priority* ; deprecated; same as *task-priority*
           #:emergency-kill-tasks   ; deprecated; same as kill-tasks
           #:kernel-handler-bind))  ; deprecated; same as task-handler-bind

(defpackage #:lparallel.kernel-util
  (:use #:cl
        #:lparallel.util
        #:lparallel.kernel
        #:lparallel.queue
        #:lparallel.counter)
  (:export #:with-submit-counted
           #:submit-counted
           #:receive-counted)
  (:export #:with-submit-dynamic-counted
           #:submit-dynamic-counted
           #:receive-dynamic-counted)
  (:export #:with-submit-indexed
           #:submit-indexed
           #:receive-indexed)
  (:export #:with-submit-cancelable
           #:submit-cancelable
           #:receive-cancelables))

(defpackage #:lparallel.ptree
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.kernel)
  (:export #:ptree
           #:ptree-fn
           #:make-ptree
           #:check-ptree
           #:call-ptree
           #:*ptree-node-kernel*)
  (:export #:ptree-undefined-function-error
           #:ptree-lambda-list-keyword-error
           #:ptree-redefinition-error))

(defpackage #:lparallel.promise
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

(defpackage #:lparallel.cognate
  (:use #:cl
        #:lparallel.util
        #:lparallel.kernel
        #:lparallel.kernel-util
        #:lparallel.promise)
  (:export #:pand
           #:pcount
           #:pcount-if
           #:pcount-if-not
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
           #:psort)
  (:export #:preduce/partial)) ; deprecated; same as preduce-partial

(defpackage #:lparallel.defpun
  (:use #:cl
        #:lparallel.util
        #:lparallel.kernel
        #:lparallel.thread-util
        #:lparallel.promise
        #:lparallel.cognate)
  (:export #:defpun))

(macrolet ((define-merged-package (name packages)
             `(defpackage ,name
                (:use ,@packages)
                (:export
                 ,@(loop
                      :for pkg :in packages
                      :nconc (loop
                                :for sym :being :the :external-symbols :in pkg
                                :collect (make-symbol (string sym))))))))
  (define-merged-package #:lparallel
      (#:lparallel.kernel
       #:lparallel.promise
       #:lparallel.cognate
       #:lparallel.defpun
       #:lparallel.ptree)))
