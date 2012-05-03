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

(in-package #:lparallel.queue)

(defslots queue ()
  ((impl :reader impl                      :type raw-queue)
   (lock :reader lock :initform (make-lock))
   (cvar              :initform nil))
  (:documentation
   "A FIFO queue."))

;;; Queues were originally vector-based, hence the `initial-capacity'.
;;; Keep the parameter in order to leave the option open.
(defun make-queue (&optional initial-capacity)
  (make-queue-instance :impl (make-raw-queue initial-capacity)))

(defmacro with-locked-queue (queue &body body)
  `(with-lock-held ((lock ,queue))
     ,@body))

(define-locking-fn push-queue (object queue) (t queue) null lock
  (with-queue-slots (impl cvar) queue
    (push-raw-queue object impl)
    (when cvar
      (condition-notify-and-yield cvar)))
  nil)

(define-locking-fn pop-queue (queue) (queue) t lock
  (with-queue-slots (impl lock cvar) queue
    (loop (multiple-value-bind (value presentp) (pop-raw-queue impl)
            (if presentp
                (return value)
                (condition-wait (or cvar (setf cvar (make-condition-variable)))
                                lock))))))

(defun/type try-pop-queue (queue) (queue) (values t boolean)
  (declare #.*normal-optimize*)
  (with-queue-slots (impl lock) queue
    (with-lock-predicate/wait lock (not (raw-queue-empty-p impl))
      (return-from try-pop-queue (pop-raw-queue impl)))
    (values nil nil)))

(defun/inline try-pop-queue/no-lock (queue)
  (pop-raw-queue (impl queue)))

(defmacro define-simple-queue-fn (name raw arg-types return-type)
  (with-gensyms (queue)
    `(define-simple-locking-fn ,name (,queue) ,arg-types ,return-type lock
       (,raw (impl ,queue)))))

(defmacro define-simple-queue-fns (&rest defs)
  `(progn ,@(loop
               :for def :in defs
               :collect `(define-simple-queue-fn ,@def))))

(define-simple-queue-fns
    (queue-count   raw-queue-count   (queue) raw-queue-count)
    (queue-empty-p raw-queue-empty-p (queue) boolean)
  (peek-queue    peek-raw-queue    (queue) (values t boolean)))

(defmacro fn-doc (fn doc)
  `(setf (documentation ',fn 'function) ,doc))

(defmacro fn-docs (&rest specs)
  (let1 no-lock-ctl "Like `~a' but does not acquire the lock. ~
                     Use inside~%`with-locked-queue'."
    `(progn ,@(loop
                 :for (fn doc) :in specs
                 :for down-fn := (string-downcase (string fn))
                 :for no-lock-doc := (format nil no-lock-ctl down-fn)
                 :for no-lock := (intern-conc fn '#:/no-lock)
                 :collect `(progn
                             (fn-doc ,fn ,doc)
                             (when (fboundp ',no-lock)
                               (fn-doc ,no-lock ,no-lock-doc)))))))

(fn-docs
 (make-queue
  "Create a queue. 

As an optimization, an internal size may be given with
`initial-capacity'. This does not affect `queue-count' and does not
limit the queue size.")

 (peek-queue
  "If `queue' is non-empty, return (values element t) where `element'
is the frontmost element of `queue'.

If `queue' is empty, return (values nil nil).")

 (push-queue
  "Push `object' onto the back of `queue'.")

 (pop-queue
  "Remove the frontmost element from `queue' and return it.

If `queue' is empty, block until an element is available.")

 (try-pop-queue
  "Non-blocking version of `pop-queue'.

If `queue' is non-empty, remove the frontmost element from `queue' and
return (values element t) where `element' is the element removed.

If `queue' is empty, return (values nil nil).")

 (queue-count
  "Return the number of elements in `queue'.")

 (queue-empty-p
  "Return true if `queue' is empty, otherwise return false.")

 (with-locked-queue
  "Execute `body' with the queue lock held. Use the `/no-lock'
functions inside `body'."))
