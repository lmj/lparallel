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

;;; Future-based `plet'.

;;; Declaration types are allowed inside `plet', mainly for
;;; compatibility with the `plet' in `defpun'. Here they don't matter
;;; as much, but considering that we need to identify the type
;;; declarations anyway, we might as well use them.

(in-package #:lparallel.cognate)

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

  (defun custom-declaration-p (symbol env)
    (member symbol (declaration-information 'declaration env))))

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

(defun declarationp (symbol env)
  (or (member symbol *standard-declaration-identifiers*)
      (custom-declaration-p symbol env)))

;;;; plet

;;; Terminology:
;;;
;;; declares: ((DECLARE FOO BAR) (DECLARE BAZ))
;;; corresponding declaration specifiers: (FOO BAR BAZ)

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
  (loop for decl-spec in decl-specs
        if (decl-spec->typed-vars decl-spec env) append it into typed-vars
        else collect decl-spec into non-type-decl-specs
        finally (return (values typed-vars non-type-decl-specs))))

(defun declares->decl-specs (declares)
  (loop for (first . rest) in declares
        do (assert (eq 'declare first))
        append rest))

(defun declares->typed-vars (declares env)
  (decl-specs->typed-vars (declares->decl-specs declares) env))

(defslots binding-datum ()
  (future-result
   future-var
   form
   (vars :reader binding-datum-vars)))

(defun make-sv-binding-datum (sv-binding)
  (destructuring-bind ((var) form) sv-binding
    (make-binding-datum-instance
     :vars (list var)
     :form `(nth-value 0 ,form)
     :future-result var
     :future-var (gensym (symbol-name var)))))

(defun make-mv-binding-datum (mv-binding)
  (destructuring-bind (vars form) mv-binding
    (flet ((sym (prefix)
             (gensym (format nil "~a/~{~a.~}" prefix vars))))
      (make-binding-datum-instance
       :vars vars
       :form form
       :future-result (sym '#:future-result)
       :future-var (sym '#:future-var)))))

(defun partition (predicate list)
  (loop for x in list
        if (funcall predicate x) collect x into pass
        else collect x into fail
        finally (return (values pass fail))))

(defun make-binding-data (bindings)
  (multiple-value-bind (normal-bindings null-bindings) (parse-bindings bindings)
    (multiple-value-bind (sv-bindings mv-bindings)
        (partition (lambda (binding) (= (length (first binding)) 1))
                   normal-bindings)
      (values (mapcar #'make-mv-binding-datum mv-bindings)
              (mapcar #'make-sv-binding-datum sv-bindings)
              null-bindings))))

(defun lookup-all (item alist &key (test #'eql))
  (loop for (x . y) in alist
        when (funcall test x item) collect y))

(defun var-type (var typed-vars)
  `(and ,@(lookup-all var typed-vars)))

(defun future-let-binding (binding-datum)
  (with-binding-datum-slots (future-var form) binding-datum
    `(,future-var (future ,form))))

(defun future-let-bindings (binding-data)
  (mapcar #'future-let-binding binding-data))

(defun future-macrolet-binding (typed-vars binding-datum)
  (with-binding-datum-slots (future-var future-result) binding-datum
    `(,future-result (the ,(var-type future-result typed-vars)
                       (force ,future-var)))))

(defun future-macrolet-bindings (typed-vars binding-data)
  (mapcar (partial-apply #'future-macrolet-binding typed-vars)
          binding-data))

(defun %mv-macrolet-bindings (typed-vars mv-binding-datum)
  (with-binding-datum-slots (vars future-result) mv-binding-datum
    (loop for var in vars
          for n from 0
          collect `(,var (the ,(var-type var typed-vars)
                           (nth-value ,n ,future-result))))))

(defun mv-macrolet-bindings (typed-vars mv-binding-data)
  (reduce #'append
          (mapcar (partial-apply #'%mv-macrolet-bindings typed-vars)
                  mv-binding-data)))

(defun binding-decl-spec (typed-vars var)
  `(type ,(var-type var typed-vars) ,var))

(defun binding-decl-specs (typed-vars vars)
  (mapcar (partial-apply #'binding-decl-spec typed-vars)
          vars))

(defun all-binding-vars (binding-data null-bindings)
  (append (reduce #'append (mapcar #'binding-datum-vars binding-data))
          null-bindings))

(defun unknown-typed-vars (typed-vars binding-data null-bindings)
  (set-difference (mapcar #'car typed-vars)
                  (all-binding-vars binding-data null-bindings)))

(defmacro %plet (bindings body &environment env)
  (with-parsed-body (body declares)
    (multiple-value-bind (typed-vars non-type-decl-specs)
        (declares->typed-vars declares env)
      (multiple-value-bind (mv-binding-data sv-binding-data null-bindings)
          (make-binding-data bindings)
        (let ((binding-data (append sv-binding-data mv-binding-data)))
          (when-let (vars (unknown-typed-vars typed-vars binding-data
                                              null-bindings))
            (warn "In type declaration for `plet', unrecognized: ~{~s ~^~}"
                  vars))
          `(let ,(future-let-bindings binding-data)
             (symbol-macrolet ,(future-macrolet-bindings typed-vars
                                                         binding-data)
               (symbol-macrolet ,(mv-macrolet-bindings typed-vars
                                                       mv-binding-data)
                 (let ,null-bindings
                   (declare ,@non-type-decl-specs
                            ,@(binding-decl-specs typed-vars null-bindings))
                   ,@body)))))))))

(defmacro plet (bindings &body body)
  "The syntax of `plet' matches that of `let'.

  plet ({var-no-init | (var [init-form]) | ((var1 var2 ...) [init-form])}*)
    declaration* form*

For each (var init-form) pair, a future is created which executes
`init-form'. Inside `body', `var' is a symbol macro which expands to a
`force' form for the corresponding future.

Likewise, each ((var1 var2 ...) init-form) pair creates a future where
`var1', `var2',... are bound to the respective multiple return values
of `init-form'.

Each `var-no-init' is bound to nil and each variable without a
corresponding `init-form' is bound to nil (no future is created).

Type declarations for vars are recognized by `plet' and incorporated
into the final expansion. The semantics of these declarations are the
same as those of a regular `let' form.

`plet' is subject to optimization inside `defpun'."
  `(%plet ,bindings ,body))

(defmacro plet-if (predicate bindings &body body)
  "The syntax of `plet-if' matches that of `plet' except for the
addition of the `predicate' form.

If `predicate' evaluates to true, the behavior is the same as `plet'.

If `predicate' evaluates to false, the behavior is the same as `slet'.

`plet-if' is subject to optimization inside `defpun'."
  `(if ,predicate
       (plet ,bindings ,@body)
       (slet ,bindings ,@body)))

(alias-macro toplevel-plet plet)

(defmacro pfuncall (function &rest args)
  "Parallel version of `funcall'. Arguments in `args' may be executed
in parallel, though not necessarily at the same time."
  (let ((vars (loop for index below (length args)
                    collect (gensym (format nil "~a-~a-"
                                            '#:pfuncall-arg index)))))
    `(toplevel-plet ,(mapcar #'list vars args)
       (funcall ,function ,@vars))))
