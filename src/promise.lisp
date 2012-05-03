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

(in-package #:lparallel.promise)

#.(import '(lparallel.kernel::unwrap-result
            lparallel.kernel::make-task-fn
            lparallel.kernel::make-task
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

(defmethod fulfilledp ((promise promise-base))
  (not (eq (result promise) 'no-result)))

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

(defgeneric fulfill-hook (promise values))
(defgeneric force-hook (promise))

(defmacro/once fulfill (&once promise &body body)
  "Fulfill a promise.

If the promise is not yet fulfilled (or if it is currently being
fulfilled) then the implicit progn `body' will be executed and the
promise will be fulfilled with the result. In this case `fulfill'
returns true.

If the promise is already fulfilled then `body' will not be executed
and `fulfill' returns false."
  `(typecase ,promise
     (promise-base
      (with-unfulfilled/no-wait ,promise
        (fulfill-hook ,promise (multiple-value-list (progn ,@body)))
        t))
     (otherwise
      ;; non-promises are always fulfilled
      nil)))

(defmethod force ((promise promise-base))
  (declare #.*normal-optimize*)
  (with-unfulfilled/wait promise
    (force-hook promise))
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

(defslots promise (promise-base)
  ((cvar :initform nil)))

(defun promise ()
  "Create a promise. A promise is a receptacle for a result which is
unknown at the time it is created."
  (make-promise-instance))

(defmethod fulfill-hook ((promise promise) values)
  (with-promise-slots (result cvar) promise
    (setf result values)
    (when cvar
      (condition-notify-and-yield cvar))))

(defmethod force-hook ((promise promise))
  (with-promise-slots (lock cvar) promise
    (condition-wait (or cvar (setf cvar (make-condition-variable))) lock)
    (condition-notify-and-yield cvar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; plan
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots plan (promise-base)
  ((fn :type (or null function))))

(defun fulfill-plan (plan values)
  (with-plan-slots (result fn) plan
    (setf result values
          fn nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; future
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots future (plan)
  ((canceledp :initform nil :type boolean)))

(defun force-future (future)
  (with-future-slots (fn) future
    ;; Since computation can possibly can occur in the calling thread,
    ;; ensure that handlers are in place.
    (fulfill-plan future (multiple-value-list (call-with-task-handler fn)))))

(defmethod fulfill-hook ((future future) values)
  (with-future-slots (canceledp) future
    (setf canceledp t)
    (fulfill-plan future values)))

(defmethod force-hook ((future future))
  (with-future-slots (canceledp) future
    ;; If we are here then we've stolen the task from the kernel.
    (setf canceledp t)
    (handler-bind ((error (lambda (e)
                            (fulfill-plan future (list (wrap-error e))))))
      (force-future future))))

(defmacro with-unfulfilled-future/no-wait (future &body body)
  (with-gensyms (lock canceledp result)
    `(with-future-slots
         ((,lock lock) (,canceledp canceledp) (,result result)) ,future
       (with-lock-predicate/no-wait ,lock (and (not ,canceledp)
                                               (eq ,result 'no-result))
         ,@body))))

(defmacro make-future-task (future)
  `(macrolet ((store-error (err)
                `(with-unfulfilled-future/no-wait ,',future
                   (fulfill-plan ,',future (list ,err)))))
     (make-task :fn (lambda ()
                      (with-unfulfilled-future/no-wait ,future
                        (force-future ,future)))
                :store-error store-error)))

(defun make-future (fn)
  (declare #.*normal-optimize*)
  (check-kernel)
  (let1 future (make-future-instance :fn fn)
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
;;; delay
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots delay (plan) ())

(defmacro delay (&body body)
  "Create a delay. A delay is a promise which is fulfilled when
`force' is called upon it."
  `(make-delay-instance :fn (lambda () ,@body)))

(defmethod fulfill-hook ((delay delay) values)
  (fulfill-plan delay values))

(defmethod force-hook ((delay delay))
  (with-delay-slots (fn) delay
    (fulfill-plan delay (multiple-value-list (funcall fn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; chain
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots chain ()
  ((object)))

(defun chain (object)
  "Create a chain. A chain links objects together by relaying `force'
and `fulfilledp' calls."
  (make-chain-instance :object object))

(defmethod force ((chain chain))
  (with-chain-slots (object) chain
    (force object)))

(defmethod fulfilledp ((chain chain))
  (with-chain-slots (object) chain
    (fulfilledp object)))

(defmethod unwrap-result ((chain chain))
  (with-chain-slots (object) chain
    (unwrap-result (force object))))
