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

(in-package #:lparallel.kernel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; thread-locals
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-thread-locals *kernel-thread-locals*
  (*debugger-error* nil
   "Thread-local. Track the error inside the debugger for the
    `transfer-error' restart.")
  
  (*handler-active-p* nil
   "Thread-local. Non-nil when handlers have been established via
    `call-with-task-handler'.")

  (*client-handlers* nil
   "Thread-local. Records handlers established with
    `task-handler-bind' in the calling thread.")

  (*task-category* :default
   "Thread-local. See `kill-tasks'. Default value is `:default'.")

  (*task-priority* :default
   "Thread-local. When bound to `:low', the kernel schedules submitted
    tasks at low priority. Default value is `:default'.")

  (*worker* nil
   "Thread-local. The worker instance if inside a worker thread,
   otherwise nil."))

;;; This is managed separately due to self-reference.
(defvar *kernel* nil "Thread-local. The current kernel, or nil.")

;;; deprecated
(alias-special *kernel-task-category* *task-category* :deprecate t)
(alias-special *kernel-task-priority* *task-priority* :deprecate t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; non-thread-local
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *optimizer* nil
  "Determines which kernel optimizer plugin is active. This is
  an eql specializer for the `make-optimizer-data' generic function")

;;; On a Core-i7 3.4GHz, a single spin takes about 2.5 microseconds.
(defvar *kernel-spin-count* 2000
  "Default value of the `spin-count' argument to `make-kernel'.")

(defvar *debug-tasks-p* t
  "If true, the debugger is invoked when an error goes unhandled
  inside a task, i.e. when the handlers established by
  `task-handler-bind' (if any) do not handle it.

  If false, unhandled errors from tasks are automatically transferred
  to their parent thread (and/or any dependent threads) via the
  `transfer-error' restart. This is for convenience -- sometimes you
  wish to avoid N debugger popups arising from N errors in N worker
  threads.

  The default value of `*debug-tasks-p*' is true. 

  For local control over debugger invocation, bind a task handler:

    (task-handler-bind ((error #'invoke-debugger)) ...)

    (task-handler-bind ((error #'invoke-transfer-error)) ...)")

(defvar *lisp-exiting-p* nil
  "True if the Lisp process is exiting; for skipping auto-replacement
  of killed workers during exit.")
