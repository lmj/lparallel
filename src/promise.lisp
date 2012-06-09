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

;;; 
;;; inheritance:
;;; 
;;;               promise-base
;;;                 /     \
;;;             promise   plan
;;;                       /  \
;;;    speculation = future  delay
;;; 
;;; Class names are prefixed with % to avoid being exported.

(in-package #:lparallel.promise)

#.(import '(lparallel.kernel::unwrap-result
            lparallel.kernel::make-task-fn
            lparallel.kernel::make-task
            lparallel.kernel::task
            lparallel.kernel::call-with-task-handler
            lparallel.kernel::submit-raw-task
            lparallel.kernel::wrap-error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; generics
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric force (object)
  (:documentation
   "If `object' is a promise and the promise is fulfilled, return the
   fulfilled value (possibly multiple values). If the promise is
   unfulfilled then the call blocks until the promise is fulfilled.

   If `object' is a chain, call `force' on the chained object.

   If `object' is not a promise and not a chain, return the identical
   object passed."))

(defmethod force (object)
  "A non-promise/non-chain forces to itself."
  object)

(defgeneric fulfilledp (object)
  (:documentation
   "If `object' is a promise, return a boolean indicating whether the
   promise is fulfilled.

   If `object' is a chain, call `fulfilledp' on the chained object.

   If `object' is not a promise and not a chain, return true."))

(defmethod fulfilledp (object)
  "A non-promise/non-chain is always fulfilled."
  (declare (ignore object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; promise-base
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots promise-base ()
  ((result :reader result :initform 'no-result)
   (lock                  :initform (make-lock))))

;;; for work-stealing loop
(defun/type/inline fulfilledp/promise (promise) (promise-base) boolean
  (declare #.*full-optimize*)
  (not (eq (result promise) 'no-result)))

(defmethod fulfilledp ((promise promise-base))
  (declare #.*normal-optimize*)
  (fulfilledp/promise promise))

(defmacro with-lock-operation (operation promise &body body)
  (with-gensyms (lock result)
    `(with-promise-base-slots ((,lock lock) (,result result)) ,promise
       (,operation ,lock (eq ,result 'no-result)
         ,@body))))

(defmacro with-unfulfilled/no-wait (promise &body body)
  `(with-lock-operation with-lock-predicate/no-wait ,promise
     ,@body))

(defmacro with-unfulfilled/wait (promise &body body)
  `(with-lock-operation with-lock-predicate/wait ,promise
     ,@body))

(defgeneric fulfill-hook (promise client-fn)
  (:method (object client-fn)
    (declare (ignore object client-fn))
    nil))

(defmacro fulfill (promise &body body)
  "Fulfill a promise.

If the promise is not yet fulfilled and if it is not currently being
fulfilled, then the implicit progn `body' will be executed and the
promise will store the result. In this case `fulfill' returns true.

If the promise is already fulfilled, or if it actively being
fulfilled, then `body' will not be executed and `fulfill' returns
false.

If a non-promise is passed then false is returned immediately, with
`body' being ignored."
  `(fulfill-hook ,promise (lambda () ,@body)))

(defgeneric force-hook (promise))

(defmethod force ((promise promise-base))
  (declare #.*normal-optimize*)
  (force-hook promise)
  (with-promise-base-slots (result) promise
    (restart-case (apply #'values (unwrap-result (first result)) (rest result))
      (store-value (&rest values)
        :report "Set promise value(s)."
        :interactive (lambda () (interact "Set promise value(s): "))
        (values-list (setf result values))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; promise
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots %promise (promise-base)
  ((cvar       :initform nil)
   (availablep :initform t :type boolean)))

(defun promise ()
  "Create a promise. A promise is a receptacle for a result which is
unknown at the time it is created."
  (make-%promise-instance))

(defmethod fulfill-hook ((promise %promise) client-fn)
  (with-%promise-slots (result lock cvar availablep) promise
    ;; spin until a promise claims it
    (loop
       :do (with-unfulfilled/no-wait promise
             (setf availablep nil)
             (setf result (multiple-value-list (funcall client-fn)))
             (when cvar (condition-notify-and-yield cvar))
             (return t))
       :while availablep)))

(defmethod force-hook ((promise %promise))
  (with-unfulfilled/wait promise
    (with-%promise-slots (result lock cvar forcedp) promise
      (unless cvar
        (setf cvar (make-condition-variable)))
      (loop
         :do (condition-wait cvar lock)
         :while (eq result 'no-result))
      (condition-notify-and-yield cvar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; plan
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A plan does not need an availablep flag because any failure to
;;; acquire the lock implies the plan is unavailable.

(defslots plan (promise-base)
  ((fn :reader plan-fn :type (or null function))))

(defun/type/inline fulfill-plan/values (plan values) (plan list) t
  (with-plan-slots (result fn) plan
    (setf result values
          fn nil)))

(defun/type/inline fulfill-plan/call (plan) (plan) t
  (fulfill-plan/values
   plan (multiple-value-list (call-with-task-handler (plan-fn plan)))))

(defun/type fulfill-plan/error (plan err) (plan (or symbol error)) t
  (fulfill-plan/values plan (list (wrap-error err))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; delay
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots %delay (plan) ())

(defmacro delay (&body body)
  "Create a delay. A delay is a promise which is fulfilled when
`force' is called upon it."
  `(make-%delay-instance :fn (lambda () ,@body)))

(defmethod fulfill-hook ((delay %delay) client-fn)
  (with-unfulfilled/no-wait delay
    (fulfill-plan/values delay (multiple-value-list (funcall client-fn)))
    t))

(defmethod force-hook ((delay %delay))
  ;; do not use task handler
  (with-unfulfilled/wait delay
    (fulfill-plan/values
     delay (multiple-value-list (funcall (plan-fn delay))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; future
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots %future (plan)
  ((canceledp :initform nil :type boolean)))

(defmethod fulfill-hook ((future %future) client-fn)
  (with-unfulfilled/no-wait future
    ;; If we are here then we've stolen the task from the kernel.
    (with-%future-slots (canceledp) future
      (setf canceledp t)
      (fulfill-plan/values future (multiple-value-list (funcall client-fn))))
    t))

(defmethod force-hook ((future %future))
  (with-unfulfilled/wait future
    ;; If we are here then we've stolen the task from the kernel.
    (with-%future-slots (canceledp) future
      (setf canceledp t)
      (fulfill-plan/call future))))

(defmacro with-unfulfilled-future/no-wait (future &body body)
  (with-gensyms (lock canceledp result)
    `(with-%future-slots
         ((,lock lock) (,canceledp canceledp) (,result result)) ,future
       (with-lock-predicate/no-wait ,lock (and (not ,canceledp)
                                               (eq ,result 'no-result))
         ,@body))))

(defun/type make-future-task (future) (%future) task
  (make-task
    (lambda ()
      (with-unfulfilled-future/no-wait future
        (unwind-protect/ext
         :main  (fulfill-plan/call future)
         ;; the task handler handles everything; unwind means thread kill
         :abort (fulfill-plan/error future 'task-killed-error))))))

(defun make-future (fn)
  (declare #.*normal-optimize*)
  (check-kernel)
  (let1 future (make-%future-instance :fn fn)
    (submit-raw-task (make-future-task future) *kernel*)
    future))

(defmacro future (&body body)
  "Create a future. A future is a promise which is fulfilled in
parallel by the implicit progn `body'.

If `force' is called on an unfulfilled future then the future is
fulfilled by the caller of `force'."
  `(make-future (make-task-fn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; speculate
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro speculate (&body body)
  "Create a speculation. A speculation is a low-priority future."
  `(let1 *task-priority* :low
     (future ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; chain
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots %chain ()
  ((object :reader chain-object)))

(defun chain (object)
  "Create a chain. A chain links objects together by relaying `force'
and `fulfilledp' calls."
  (make-%chain-instance :object object))

(defmethod force ((chain %chain))
  (force (chain-object chain)))

(defmethod fulfilledp ((chain %chain))
  (fulfilledp (chain-object chain)))

(defmethod fulfill-hook ((chain %chain) client-fn)
  (fulfill-hook (chain-object chain) client-fn))

(defun force/no-restart (promise)
  (force-hook promise)
  (values-list (result promise)))

(defmethod unwrap-result ((chain %chain))
  (unwrap-result (force/no-restart (chain-object chain))))
