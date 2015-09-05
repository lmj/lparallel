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
           #:chain)
  (:import-from #:lparallel.kernel
                #:unwrap-result
                #:task-lambda
                #:make-task
                #:task
                #:call-with-task-handler
                #:submit-raw-task
                #:wrap-error
                #:wrapped-error))

(in-package #:lparallel.promise)

;;;; classes

;;;
;;;               promise-base
;;;                 /     \
;;;             %promise  plan
;;;                       /  \
;;;    speculation = %future  %delay
;;;
;;; some class names are prefixed with % to avoid being exported

(defconstant +no-result+ 'no-result)

(defslots promise-base ()
  ((result :reader result :initform +no-result+)
   (lock                  :initform (make-lock))))

(defslots %promise (promise-base)
  ((cvar       :initform nil)
   (availablep :initform t :type boolean)))

;;; A plan does not need an availablep flag because any failure to
;;; acquire the lock implies the plan is unavailable.
(defslots plan (promise-base)
  ((fn :reader plan-fn :type (or null function))))

(defslots %future (plan)
  ((canceledp :initform nil :type boolean)))

(defslots %delay (plan) ())

(defslots %chain ()
  ((object :reader chain-object)))

;;;; macros

(defmacro with-lock-operation (operation promise &body body)
  (with-gensyms (lock result)
    `(with-promise-base-slots ((,lock lock) (,result result)) ,promise
       (,operation ,lock (eq ,result +no-result+)
         ,@body))))

(defmacro with-unfulfilled/no-wait (promise &body body)
  `(with-lock-operation with-lock-predicate/no-wait ,promise
     ,@body))

(defmacro with-unfulfilled/wait (promise &body body)
  `(with-lock-operation with-lock-predicate/wait ,promise
     ,@body))

;;;; promise

(defun promise ()
  "Create a promise. A promise is a receptacle for a result which is
unknown at the time it is created."
  (make-%promise-instance))

(defun fulfill-promise (promise client-fn)
  (with-%promise-slots (result lock cvar availablep) promise
    ;; spin until it is claimed
    (loop while availablep
          do (with-unfulfilled/no-wait promise
               ;; client-fn could be expensive; set availablep in the meantime
               (unwind-protect/ext
                :prepare (setf availablep nil)
                :main    (setf result (multiple-value-list (funcall client-fn)))
                :abort   (setf availablep t))
               (when cvar
                 (condition-notify cvar))
               (return t)))))

(defun force-promise (promise)
  (with-%promise-slots (result lock cvar) promise
    (unless cvar
      (setf cvar (make-condition-variable)))
    (loop while (eq result +no-result+)
          do (condition-wait cvar lock))
    (condition-notify cvar)))

;;;; plan

(defun/inline fulfill-plan/values (plan values)
  (with-plan-slots (result fn) plan
    (setf result values
          fn nil)))

(defun/inline fulfill-plan/call (plan)
  (fulfill-plan/values
   plan (multiple-value-list (call-with-task-handler (plan-fn plan)))))

(defun fulfill-plan/error (plan err)
  (fulfill-plan/values plan (list (wrap-error err))))

;;;; delay

(defmacro delay (&body body)
  "Create a delay. A delay is a promise which is fulfilled when
`force' is called upon it."
  `(make-%delay-instance :fn (lambda () ,@body)))

(defun fulfill-delay (delay client-fn)
  (with-unfulfilled/no-wait delay
    (fulfill-plan/values delay (multiple-value-list (funcall client-fn)))
    t))

(defun force-delay (delay)
  ;; do not use task handler
  (fulfill-plan/values
   delay (multiple-value-list (funcall (plan-fn delay)))))

;;;; future

(defun fulfill-future (future client-fn)
  (with-unfulfilled/no-wait future
    ;; If we are here then we've stolen the task from the kernel.
    (with-%future-slots (canceledp) future
      (setf canceledp t)
      (fulfill-plan/values future (multiple-value-list (funcall client-fn))))
    t))

(defun force-future (future)
  ;; If we are here then we've stolen the task from the kernel.
  (with-%future-slots (canceledp) future
    (setf canceledp t)
    (fulfill-plan/call future)))

(defmacro with-unfulfilled-future/no-wait (future &body body)
  (with-gensyms (lock canceledp result)
    `(with-%future-slots
         ((,lock lock) (,canceledp canceledp) (,result result)) ,future
       (with-lock-predicate/no-wait ,lock (and (not ,canceledp)
                                               (eq ,result +no-result+))
         ,@body))))

(defun/type make-future-task (future) (%future) task
  (declare #.*full-optimize*)
  (make-task
    (lambda ()
      (with-unfulfilled-future/no-wait future
        (unwind-protect/ext
         :main  (fulfill-plan/call future)
         ;; the task handler handles everything; unwind means thread kill
         :abort (fulfill-plan/error future 'task-killed-error))))))

(defun/type make-future (fn) (function) %future
  (declare #.*normal-optimize*)
  (let ((kernel (check-kernel))
        (future (make-%future-instance :fn fn)))
    (submit-raw-task (make-future-task future) kernel)
    future))

(defmacro future (&body body)
  "Create a future. A future is a promise which is fulfilled in
parallel by the implicit progn `body'."
  `(make-future (task-lambda ,@body)))

;;;; speculate

(defmacro speculate (&body body)
  "Create a speculation. A speculation is a low-priority future."
  `(let ((*task-priority* :low))
     (future ,@body)))

;;;; chain

(defun chain (object)
  "Create a chain. A chain links objects together by relaying `force'
and `fulfilledp' calls."
  (make-%chain-instance :object object))

;;;; fulfill, fulfilledp, force

(defun fulfill-object (object client-fn)
  (typecase object
    (%future   (fulfill-future object client-fn))
    (%promise  (fulfill-promise object client-fn))
    (%delay    (fulfill-delay object client-fn))
    (%chain    (fulfill-object (chain-object object) client-fn))
    (otherwise nil)))

(defmacro fulfill (object &body body)
  "Attempt to give `object' a value.

If `object' is a promise which is not fulfilled and not currently
being fulfilled, then the implicit progn `body' will be executed and
the promise will store the result. In this case `fulfill' returns
true.

If `object' is a promise that is either already fulfilled or actively
being fulfilled, then `body' will not be executed and `fulfill'
returns false.

If `object' is a chain, call `fulfill' on the chained object.

If `object' is not a promise and not a chain then false is returned
immediately, with `body' being ignored."
  `(fulfill-object ,object (lambda () ,@body)))

(defun fulfilledp (object)
   "If `object' is a promise, return a boolean indicating whether the
promise is fulfilled.

If `object' is a chain, call `fulfilledp' on the chained object.

If `object' is not a promise and not a chain, return true."
  (declare #.*normal-optimize*)
  (typecase object
    (promise-base (not (eq (result object) +no-result+)))
    (%chain       (fulfilledp (chain-object object)))
    (otherwise    t)))

(defun replace-error (promise)
  ;; It is not possible to return from `force' while the promise
  ;; contains an error. Therefore we do not violate the
  ;; one-result-only constraint by replacing a wrapped error result
  ;; with value(s).
  ;;
  ;; If a successful store-value invocation happens concurrently then
  ;; skip.
  (with-promise-base-slots (result lock) promise
    (with-lock-predicate/wait lock (typep (first result) 'wrapped-error)
      (restart-case (unwrap-result (first result))
        (store-value (&rest values)
          :report "Set promise value(s)."
          :interactive (lambda () (interact "Promise value(s): "))
          (setf result values))))))

(defun force (object)
  "If `object' is a promise and the promise is fulfilled, return the
fulfilled value (possibly multiple values). If the promise is
unfulfilled then the call blocks until the promise is fulfilled.

If `object' is a chain, call `force' on the chained object.

If `object' is not a promise and not a chain, return the identical
object passed.

Note if `force' is called on an unfulfilled future then the future is
fulfilled by the caller of `force'."
  (declare #.*normal-optimize*)
  (typecase object
    (promise-base
     (with-unfulfilled/wait object
       (etypecase object
         (%future  (force-future object))
         (%promise (force-promise object))
         (%delay   (force-delay object))))
     ;; result must now be a list
     (let ((result (result object)))
       (typecase (first result)
         (wrapped-error (replace-error object)
                        (force object))
         (%chain (force (chain-object (first result))))
         (otherwise (values-list result)))))
    (%chain (force (chain-object object)))
    (otherwise object)))
