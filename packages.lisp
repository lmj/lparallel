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

(defpackage #:lparallel.util
  (:use #:cl)
  (:export #:with-gensyms
           #:defmacro/once
           #:mklist
           #:unsplice
           #:intern-conc)
  (:export #:while
           #:repeat
           #:when-let
           #:while-let
           #:rebind
           #:let1
           #:alias-function)
  (:export #:defun/inline
           #:defun/ftype)
  (:export #:defslots)
  (:export #:interact)
  (:export #:*normal-optimize*
           #:*full-optimize*))

(defpackage #:lparallel.thread-util
  (:use #:cl
        #:lparallel.util)
  ;; re-exported (just for convenience)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:make-lock
                #:make-recursive-lock
                #:make-condition-variable
                #:with-lock-held
                #:with-recursive-lock-held
                #:acquire-lock
                #:release-lock
                #:condition-wait)
  ;; not re-exported
  (:import-from #:bordeaux-threads
                #:condition-notify
                #:thread-yield
                #:*default-special-bindings*)
  (:export #:with-thread
           #:with-lock-predicate/wait
           #:with-lock-predicate/no-wait
           #:define-thread-locals
           #:define-locking-fn
           #:define-simple-locking-fn
           #:condition-notify-and-yield)
  (:export #:make-thread
           #:make-lock
           #:make-recursive-lock
           #:make-condition-variable
           #:with-lock-held
           #:with-recursive-lock-held
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
           #:push-counter #:push-counter/no-lock
           #:pop-counter  #:pop-counter/no-lock
           #:peek-counter #:peek-counter/no-lock
           #:with-locked-counter))

(defpackage #:lparallel.kernel
  (:use #:cl
        #:lparallel.util
        #:lparallel.thread-util
        #:lparallel.queue
        #:lparallel.biased-queue)
  (:import-from #:bordeaux-threads
                #:destroy-thread)
  (:intern #:unwrap-result
           #:make-client-fn
           #:make-task
           #:call-with-kernel-handler
           #:submit-raw-task)
  (:intern #:*kernel-thread-locals*)
  (:export #:make-kernel
           #:kernel-worker-count
           #:check-kernel
           #:end-kernel
           #:kernel-handler-bind
           #:kernel-special-bindings)
  (:export #:channel
           #:make-channel
           #:submit-task
           #:submit-timeout
           #:receive-result
           #:do-fast-receives
           #:emergency-kill-tasks ; deprecated -- same as kill-tasks
           #:kill-tasks)
  (:export #:*kernel*
           #:*kernel-task-category*
           #:*kernel-task-priority*)
  (:export #:transfer-error
           #:no-kernel-error
           #:task-killed-error))

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
  (:import-from #:lparallel.kernel
                #:*kernel-thread-locals*)
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
  (:import-from #:lparallel.kernel
                #:unwrap-result
                #:make-client-fn
                #:make-task
                #:call-with-kernel-handler
                #:submit-raw-task)
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
  (:export #:psort
           #:preduce
           #:preduce/partial
           #:pmap
           #:pmap-into
           #:pmap-reduce
           #:pmapcar
           #:pmapc
           #:pmapcan
           #:pmaplist
           #:pmaplist-into
           #:pmapl
           #:pmapcon
           #:premove
           #:premove-if
           #:premove-if-not
           #:pevery
           #:psome
           #:pnotevery
           #:pnotany
           #:plet
           #:plet*
           #:pand
           #:por))

(macrolet ((define-merged-package (name packages)
             `(defpackage ,name
                (:use #:cl ,@packages)
                (:export
                 ,@(loop
                      :for pkg :in packages
                      :nconc (loop
                                :for sym :being :the :external-symbols :in pkg
                                :collect (make-symbol (string sym))))))))
  (define-merged-package
      #:lparallel (#:lparallel.kernel
                   #:lparallel.promise
                   #:lparallel.cognate
                   #:lparallel.ptree)))
