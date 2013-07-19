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

(in-package #:lparallel.defpun)

(import-now alexandria:simple-style-warning
            lparallel.util::symbolicate/package
            lparallel.kernel::*worker*
            lparallel.kernel::*make-limiter-data*
            lparallel.kernel::kernel
            lparallel.kernel::unwrap-result
            lparallel.kernel::wrap-error
            lparallel.kernel::make-task
            lparallel.kernel::make-task-fn
            lparallel.kernel::submit-raw-task
            lparallel.kernel::accept-task-p
            lparallel.kernel::use-caller-p
            lparallel.kernel::limiter-data
            lparallel.kernel::with-limiter-slots
            lparallel.kernel::steal-work
            lparallel.kernel::call-with-task-handler)

;;;; util

(defmacro with-lock-predicate/wait*
    (&key lock predicate1 predicate2 succeed/lock succeed/no-lock fail)
  (with-gensyms (top fail-tag)
    `(block ,top
       (tagbody
          (when ,predicate1
            (with-spin-lock-held (,lock)
              (if ,predicate2
                  ,succeed/lock
                  (go ,fail-tag)))
            (return-from ,top ,succeed/no-lock))
        ,fail-tag
          (return-from ,top ,fail)))))

(defun has-lambda-list-keyword-p (list)
  (some (lambda (elem) (find elem lambda-list-keywords)) list))

(defmacro defun/wrapper (wrapper-name impl-name params &body body)
  (with-gensyms (args kernel)
    (multiple-value-bind (wrapper-params expansion)
        (if (has-lambda-list-keyword-p params)
            (values `(&rest ,args)
                    ``(apply (function ,',impl-name) ,,kernel ,',args))
            (values params
                    ``(,',impl-name ,,kernel ,@',params)))
      `(defun ,wrapper-name ,wrapper-params
         (macrolet ((call-impl (,kernel) ,expansion))
           ,@body)))))

;;;; lightweight futures

(defconstant +no-result+ 'no-result)

(deftype future () 'cons)

(defmacro result (future)
  `(car (the future ,future)))

(defmacro future-fn (future)
  `(the function (cdr (the future ,future))))

(defmacro %make-future (fn)
  `(cons +no-result+ ,fn))

(defmacro fulfilledp (future)
  `(not (eq (result ,future) +no-result+)))

(defmacro force (future)
  `(locally (declare #.*full-optimize*)
     (unwrap-result (result ,future))))

(defun/type make-future (kernel fn) (kernel function) future
  (declare #.*full-optimize*)
  (let ((future (%make-future fn)))
    (submit-raw-task
     (make-task
      (lambda ()
        (unwind-protect/ext
         :main  (setf (result future) (call-with-task-handler
                                       (future-fn future)))
         :abort (setf (result future) (wrap-error 'task-killed-error)))))
     kernel)
    future))

(defmacro future (kernel &body body)
  `(make-future ,kernel (make-task-fn ,@body)))

;;;; declarationp

;;; `declaration-information' resolves the ambiguity between types and
;;; custom declares -- (declare (type foo x)) may be abbreviated as
;;; (declare (foo x)).
#+lparallel.with-cltl2
(progn
  #-(or sbcl ccl lispworks allegro)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (error "cltl2 not (yet?) enabled for this implementation."))

  (defun declaration-information (decl env)
    (#+sbcl sb-cltl2:declaration-information
     #+ccl ccl:declaration-information
     #+lispworks hcl:declaration-information
     #+allegro sys:declaration-information
     decl env))

  (defun custom-declaration-p (form env)
    (member form (declaration-information 'declaration env))))

;;; When `declaration-information' is not available use `subtypep'
;;; instead. On implementations that have a weak `subtypep', a deftype
;;; that expands to a compound type might not be recognized as a type.
;;; There's no way to solve this portably. The user can avoid this
;;; problem by using the literal `type' declaration instead of
;;; omitting `type' as shortcut.
#-lparallel.with-cltl2
(progn
  (defun known-type-p (symbol)
    (ignore-errors (nth-value 1 (subtypep symbol nil))))

  (defun custom-declaration-p (form env)
    (declare (ignore env))
    (typecase form
      (symbol (not (known-type-p form))))))

(defparameter *standard-declaration-identifiers*
  '(dynamic-extent  ignore     optimize
    ftype           inline     special
    ignorable       notinline  type))

(defun declarationp (form env)
  (or (member form *standard-declaration-identifiers*)
      (custom-declaration-p form env)))

;;;; future-let

;;; Terminology:
;;;
;;; declares: ((DECLARE FOO BAR) (DECLARE BAZ))
;;; corresponding declaration specifiers (decl-specs): (FOO BAR BAZ)

(defun zip-repeat (fn list object)
  (mapcar (lambda (elem) (funcall fn elem object)) list))

(defun decl-spec->typed-vars (decl-spec env)
  (destructuring-bind (head &rest list) decl-spec
    (cond ((eq head 'type)
           (destructuring-bind (type &rest vars) list
             (zip-repeat #'cons vars type)))
          ((declarationp head env)
           nil)
          (t
           ;; (foo x) shorthand for (type foo x)
           (zip-repeat #'cons list head)))))

(defun decl-specs->typed-vars (decl-specs env)
  (loop
     :for decl-spec :in decl-specs
     :if (decl-spec->typed-vars decl-spec env) :append it :into typed-vars
     :else :collect decl-spec :into non-type-decl-specs
     :finally (return (values typed-vars non-type-decl-specs))))

(defun declares->decl-specs (declares)
  (loop
     :for (first . rest) :in declares
     :do (assert (eq 'declare first))
     :append rest))

(defun declares->typed-vars (declares env)
  (decl-specs->typed-vars (declares->decl-specs declares) env))

(defun bindings->vars (bindings)
  (mapcar (lambda (binding)
            (etypecase binding
              (cons (first binding))
              (symbol binding)))
          bindings))

(defun unknown-typed-vars (typed-vars bindings)
  (set-difference (mapcar #'car typed-vars) (bindings->vars bindings)))

(defun pairp (form)
  (and (consp form) (eql (length form) 2)))

(defun lookup-all (item alist &key (test #'eql))
  (loop
     :for (x . y) :in alist
     :when (funcall test x item) :collect y))

(defmacro with-parsed-let-args ((pairs non-pairs syms) bindings &body body)
  (check-type bindings symbol)
  `(let* ((,pairs     (remove-if-not #'pairp ,bindings))
          (,non-pairs (remove-if     #'pairp ,bindings))
          (,syms      (loop
                         :for (name nil) :in ,pairs
                         :collect (gensym (symbol-name name)))))
     ,@body))

(defmacro future-let (&key kernel future force bindings body &environment env)
  (with-parsed-body (body declares)
    (with-parsed-let-args (pairs non-pairs syms) bindings
      (multiple-value-bind (typed-vars non-type-decl-specs)
          (declares->typed-vars declares env)
        (when-let (vars (unknown-typed-vars typed-vars bindings))
          (warn "In type declaration for `plet', unrecognized: ~{~s ~^~}" vars))
        `(symbol-macrolet
             ,(loop
                 :for sym :in syms
                 :for (name nil) :in pairs
                 :for force-form := `(,force ,@(unsplice kernel) ,sym)
                 :for types := (lookup-all name typed-vars)
                 :collect `(,name (the (and ,@types) ,force-form)))
           (let (,@(loop
                      :for sym :in syms
                      :for (nil form) :in pairs
                      :collect `(,sym (,future ,@(unsplice kernel) ,form)))
                 ,@non-pairs)
             (declare ,@non-type-decl-specs)
             ,@body))))))

;;;; function registration

;;; defpun relies upon the inlined accept-task-p call in order to
;;; achieve speedup, which means that *kernel* must exist before the
;;; call takes place. If *kernel* is nil, the error may be confusing
;;; due to inlining and optimizations. Inserting `check-kernel' into
;;; the body of defpun functions negates the speedup for small
;;; functions.
;;;
;;; Thus for user-friendliness we define checked and unchecked
;;; functions for each defpun form. The user calls the checked
;;; version; defpun calls the unchecked one via a macrolet.
;;;
;;; The macrolets also have the happy side-effect of preventing
;;; reference to the checked (slower) function via #'.
;;;
;;; We store references to the checked and unchecked functions in
;;; order to detect redefinitions with defun or otherwise.

(defconstant +checked-key+ 'checked-key)
(defconstant +unchecked-key+ 'unchecked-key)

(defvar *registered-names* nil)
(defvar *registration-lock* (make-lock))

(defun unchecked-name (name)
  ;; We could intern this into a private package and maintain an alist
  ;; of (public . private) package pairs, but that seems
  ;; over-engineered. Anonymous packages don't exist anyway.
  (symbolicate/package (symbol-package name) '#:%%%%.defpun. name) )

(defun register-name (name)
  (pushnew name *registered-names*))

(defun register-fn (name)
  (setf (get name +checked-key+) (symbol-function name))
  (setf (get name +unchecked-key+) (symbol-function (unchecked-name name))))

(defun registered-fn-p (name)
  (get name +checked-key+))

(defun valid-registered-fn-p (name)
  (and (fboundp name)
       (eq (symbol-function name)
           (get name +checked-key+))
       (fboundp (unchecked-name name))
       (eq (symbol-function (unchecked-name name))
           (get name +unchecked-key+))))

;;; a name may be registered without having a corresponding function
(defun valid-registered-name-p (name)
  (and (symbol-package name)
       (or (not (registered-fn-p name))
           (valid-registered-fn-p name))))

(defun delete-stale-registrations ()
  (setf *registered-names*
        (remove-if-not #'valid-registered-name-p *registered-names*)))

(defun registered-macrolets (kernel)
  (loop
     :for name :in *registered-names*
     :collect `(,name (&rest args)
                 `(,',(unchecked-name name) ,',kernel ,@args))))

(defmacro declaim-defpun (&rest names)
  "See `defpun'."
  ;; This is used outside of the defpun macro.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (with-lock-held (*registration-lock*)
       ,@(loop
            :for name :in names
            :collect `(register-name ',name)))))

(defun delete-registered-names (names)
  ;; This is used outside of the defpun macro.
  (with-lock-held (*registration-lock*)
    (setf *registered-names* (set-difference *registered-names* names))))

;;;; defpun

;;; Must use struct due to CAS.
(locally (declare #.*full-optimize*)
  (defstruct (limiter-data (:constructor %make-limiter-data))
    (lock (make-spin-lock))
    (count 0 :type fixnum)
    (limit (error "no limit") :type fixnum)))

(defun make-limiter-data (worker-count use-caller)
  (%make-limiter-data :limit (if use-caller
                                 (1+ worker-count)
                                 worker-count)))

(setf *make-limiter-data* 'make-limiter-data)

(defmacro accept-task-p/fast (kernel)
  (check-type kernel symbol)
  `(locally (declare #.*full-optimize*)
     (accept-task-p (the kernel ,kernel))))

(defun/type update-task-count/no-lock (kernel delta) (kernel fixnum) t
  (declare #.*full-optimize*)
  (with-limiter-slots (accept-task-p limiter-data) kernel
    (incf (limiter-data-count limiter-data) delta)
    ;; `<=' returns generalized boolean
    (setf accept-task-p
          (to-boolean (<= (limiter-data-count limiter-data)
                          (the fixnum (limiter-data-limit limiter-data)))))))

(defun/type update-task-count (kernel delta) (kernel fixnum) t
  (declare #.*full-optimize*)
  (with-spin-lock-held ((limiter-data-lock (limiter-data kernel)))
    (update-task-count/no-lock kernel delta)))

(defmacro future/fast (kernel &body body)
  `(future ,kernel
     (unwind-protect
          (progn ,@body)
       (update-task-count ,kernel -1))))

(defun/type force/fast (kernel future) (kernel future) t
  (declare #.*full-optimize*)
  (if (fulfilledp future)
      (force future)
      (let ((worker *worker*))
        (loop
           (when (fulfilledp future)
             (return (force future)))
           #+lparallel.with-green-threads (thread-yield)
           (steal-work kernel worker)))))

(defmacro %%plet/fast (kernel predicate future-count bindings body)
  `(with-lock-predicate/wait*
       :lock            (limiter-data-lock (limiter-data ,kernel))
       :predicate1      ,predicate
       :predicate2      (accept-task-p/fast ,kernel)
       :succeed/lock    (update-task-count/no-lock ,kernel ,future-count)
       :succeed/no-lock (future-let :kernel ,kernel
                                    :future future/fast
                                    :force force/fast
                                    :bindings ,bindings
                                    :body ,body)
       :fail            (let ,bindings ,@body)))

(defmacro %plet/fast (kernel predicate bindings body)
  (with-parsed-let-args (pairs non-pairs syms) bindings
    (declare (ignore non-pairs syms))
    (if pairs
        `(%%plet/fast ,kernel ,predicate ,(length pairs) ,bindings ,body)
        `(let ,bindings ,@body))))

(defmacro plet/fast (kernel bindings &body body)
  `(%plet/fast ,kernel
               (accept-task-p/fast ,kernel)
               ,bindings
               ,body))

(defmacro plet-if/fast (kernel predicate bindings &body body)
  `(%plet/fast ,kernel
               (and (accept-task-p/fast ,kernel) ,predicate)
               ,bindings ,body))

(defun call-impl-in-worker (call-impl)
  (if *worker*
      (funcall call-impl)
      (let ((channel (make-channel)))
        (submit-task channel (lambda ()
                               (multiple-value-list (funcall call-impl))))
        (values-list (receive-result channel)))))

(defmacro define-defpun (defpun doc defun &rest types)
  `(defmacro ,defpun (name params ,@types &body body)
     ,doc
     (with-parsed-body (body declares docstring)
       (with-lock-held (*registration-lock*)
         ;; these two calls may affect the registered macrolets in the
         ;; return form below
         (delete-stale-registrations)
         (register-name name)
         (with-gensyms (kernel)
           `(progn
              (,',defun ,(unchecked-name name) (,kernel ,@params)
                  ,,@(unsplice (when types ``(kernel ,@,(first types))))
                  ,,@(unsplice (when types (second types)))
                ,@declares
                (declare (ignorable ,kernel))
                (macrolet ((plet (bindings &body body)
                             `(plet/fast ,',kernel ,bindings
                                ,@body))
                           (plet-if (predicate bindings &body body)
                             `(plet-if/fast ,',kernel ,predicate ,bindings
                                ,@body))
                           ,@(registered-macrolets kernel))
                  ,@body))
              (defun/wrapper ,name ,(unchecked-name name) ,params
                ,@(unsplice docstring)
                (let ((,kernel (check-kernel)))
                  (if (use-caller-p ,kernel)
                      (call-impl ,kernel)
                      (call-impl-in-worker (lambda () (call-impl ,kernel))))))
              (eval-when (:load-toplevel :execute)
                (with-lock-held (*registration-lock*)
                  (register-fn ',name)))
              ',name))))))

(define-defpun defpun
  "`defpun' defines a function which is specially geared for
fine-grained parallelism. If you have many small tasks which bog down
the system, `defpun' may help.

The syntax of `defpun' matches that of `defun'. The difference is that
`plet' and `plet-if' take on new meaning inside `defpun'. The symbols
in the binding positions of `plet' and `plet-if' should be viewed as
lazily evaluated immutable references.

Inside a `defpun' form the name of the function being defined is a
macrolet, as are the names of other functions which were defined by
`defpun'. Thus using #' on them is an error. Calls to functions
defined by `defpun' entail more overhead when the caller lies outside
a `defpun' form.

A `defpun' function must exist before it is referenced inside another
`defpun' function. If this is not possible--for example if func1 and
func2 reference each other--then use `declaim-defpun' to specify
intent:

    (declaim-defpun func1 func2)
"
  defun)

(define-defpun defpun/type
  "Typed version of `defpun'.

`arg-types' is an unevaluated list of argument types.

`return-type' is an unevaluated form of the return type, possibly
indicating multiple values as in (values fixnum float).

\(As a technical point, if `return-type' contains no lambda list
keywords then the return type given to ftype will be additionally
constrained to match the number of return values specified.)"
  defun/type
  arg-types
  return-type)

(defmacro defpun* (&rest args)
  "Deprecated. Instead use `defpun' and pass `:use-caller t' to `make-kernel'."
  (simple-style-warning
   "`defpun*' is deprecated. Instead use `defpun' and pass ~
    `:use-caller t' to `make-kernel'.")
  `(defpun ,@args))

(defmacro defpun/type* (&rest args)
  "Deprecated. Instead use `defpun/type' and pass `:use-caller t' to
`make-kernel'."
  (simple-style-warning
   "`defpun/type*' is deprecated. Instead use `defpun/type' and pass ~
    `:use-caller t' to `make-kernel'.")
  `(defpun/type ,@args))
