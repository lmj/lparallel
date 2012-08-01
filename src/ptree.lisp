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

(in-package #:lparallel.ptree)

(import-now lparallel.kernel::kernel
            lparallel.kernel::submit-raw-task
            lparallel.kernel::with-task-context
            lparallel.kernel::make-task
            lparallel.kernel::make-task-fn
            lparallel.kernel::wrapped-error
            lparallel.kernel::wrap-error
            lparallel.kernel::unwrap-result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; errors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition ptree-error (error)
  ((id :initarg :id :reader ptree-error-id)))

(define-condition ptree-redefinition-error (ptree-error)
  ()
  (:report
   (lambda (err stream)
     (format stream "ptree function redefined: ~a" (ptree-error-id err))))
  (:documentation
   "Attempted to redefine a node's function."))

(define-condition ptree-undefined-function-error (ptree-error)
  ((refs :initarg :refs :reader ptree-error-refs))
  (:report
   (lambda (err stream)
     (format stream
             "Function not found in ptree: ~a~%Referenced by: ~{~a ~}"
             (ptree-error-id err)
             (ptree-error-refs err))))
  (:documentation
   "Attempted to execute a node which had no function."))

(define-condition ptree-lambda-list-keyword-error (ptree-error)
  ((keyword :initarg :keyword :reader ptree-error-keyword))
  (:report
   (lambda (err stream)
     (format stream
             "Function arguments in PTREE cannot contain lambda list ~
              keywords:~%Found ~a in the definition of ~a"
             (ptree-error-keyword err)
             (ptree-error-id err))))
  (:documentation
   "Lambda list keywords found in function definition."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; node
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +no-result+ 'no-result)

(defslots node ()
  ((id              :reader id)
   (function                           :initform nil :type (or function null))
   (children                           :initform nil :type list)
   (parents                            :initform nil :type list)
   (lock-level      :reader lock-level :initform 0   :type fixnum)
   (children-done-p                    :initform nil :type boolean)
   (result          :reader result     :initform +no-result+)))

(defun clear-node (node)
  (with-node-slots (lock-level children-done-p result) node
    (setf lock-level 0
          children-done-p nil
          result +no-result+)))

(defun clear-node-error (node)
  (with-node-slots (lock-level children-done-p result) node
    (setf lock-level 0
          children-done-p nil)
    (when (typep result 'wrapped-error)
      (setf result +no-result+))))

(defun check-node (node)
  (with-node-slots (id function parents) node
    (unless function
      (error 'ptree-undefined-function-error
             :id id
             :refs (mapcar #'id parents)))))

(defun/type/inline computedp (node) (node) boolean
  (declare #.*full-optimize*)
  (not (eq (result node) +no-result+)))

(defun/type compute-node (node) (node) t
  (declare #.*normal-optimize*)
  (with-node-slots (function children result) node
    (typecase function
      (function
       (unwind-protect/ext
        :main  (setf result (with-task-context
                              (apply (locally (declare #.*full-optimize*)
                                       (the function function))
                                     (mapcar #'result children))))
        :abort (setf result (wrap-error 'task-killed-error))))
      (otherwise
       (check-node node)))))

(defun/type/inline freep (node) (node) t
  (declare #.*full-optimize*)
  (zerop (lock-level node)))

(defun/type propagate-error (node error-result) (node wrapped-error) null
  (declare #.*full-optimize*)
  (with-node-slots (result parents) node
    (setf result error-result)
    (dolist (parent parents)
      (propagate-error parent error-result))))

(defun/type lock-node (node) (node) null
  (declare #.*full-optimize*)
  (with-node-slots (lock-level parents) node
    (incf lock-level)
    (dolist (parent parents)
      (lock-node parent))))

(defun/type unlock-node (node) (node) null
  (declare #.*full-optimize*)
  (with-node-slots (lock-level parents) node
    (decf lock-level)
    (dolist (parent parents)
      (unlock-node parent))))

(defun/type/inline children-done-p (node) (node) boolean
  (declare #.*full-optimize*)
  (with-node-slots (children children-done-p) node
    (or children-done-p
        (null children)
        (progn
          (dolist (child children)
            (unless (computedp child)
              (return-from children-done-p nil)))
          (setf children-done-p t)))))

(defvar *ptree-node-kernel* nil
  "When non-nil, `*kernel*' is bound to this value during the call of
  a node function.")

(defun/type make-node-task (queue node) (queue node) t
  (declare #.*normal-optimize*)
  (macrolet ((compute ()
               `(unwind-protect
                     (compute-node node)
                  (push-queue node queue))))
    (make-task
     (if *ptree-node-kernel*
         (let1 node-kernel *ptree-node-kernel*
           (make-task-fn
             (let1 *kernel* node-kernel
               (compute))))
         (make-task-fn
           (compute))))))

(defun/type submit-node (node queue kernel) (node queue kernel) null
  (declare #.*normal-optimize*)
  (let1 task (make-node-task queue node)
    (submit-raw-task task kernel))
  nil)

(defun/type find-node (node) (node) (or node null)
  (declare #.*full-optimize*)
  (with-node-slots (children) node
    (cond ((computedp node)
           ;; already computed
           nil)
          ((and (freep node) (children-done-p node))
           ;; not computed, not locked, and its children are computed;
           ;; ready to compute
           node)
          (t
           ;; not computed and either locked or children not computed;
           ;; recurse to children
           (dolist (child children)
             (when-let (found (find-node child))
               (return found)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ptree
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots %ptree ()
  ((nodes   :initform (make-hash-table :test #'eq) :type hash-table
            :reader nodes)
   (queue   :initform (make-queue)                 :type queue)
   (pending :initform 0                            :type integer)))

(defun make-ptree ()
  "Create a ptree instance."
  (make-%ptree-instance))

(defun/type compute-ptree (root ptree kernel) (node %ptree kernel) node
  (declare #.*normal-optimize*)
  (with-%ptree-slots (queue pending) ptree
    (loop (let1 node (find-node root)
            (cond (node
                   (lock-node node)
                   (incf pending)
                   (submit-node node queue kernel))
                  (t
                   (setf node (pop-queue queue))
                   (decf pending)
                   (unlock-node node)
                   (when (typep (result node) 'wrapped-error)
                     (propagate-error node (result node))
                     (return node))
                   (when (eq node root)
                     (return node))))))))

(defun wait-for-compute (ptree)
  (with-%ptree-slots (queue pending) ptree
    (while (plusp pending)
      (pop-queue queue)
      (decf pending))))

(defun each-node (ptree fn)
  (maphash (lambda (id node)
             (declare (ignore id))
             (funcall fn node))
           (nodes ptree)))

(defun check-ptree (ptree)
  "Verify that all nodes have been defined with an associated
function. If not, `ptree-undefined-function-error' is signaled."
  (each-node ptree #'check-node))

(defun clear-ptree (ptree)
  "Clear all node results in `ptree', restoring the tree to its
uncomputed state."
  (wait-for-compute ptree)
  (each-node ptree #'clear-node))

(defun clear-ptree-errors (ptree)
  "Clear all error results in `ptree', allowing the computation to
resume from its latest pre-error state."
  (wait-for-compute ptree)
  (each-node ptree #'clear-node-error))

(defun ptree-fn (id args function ptree)
  "Define a ptree node with identifier `id', which is some unique
object suitable for `eq' comparison such as symbol.

The ids of its child nodes are elements of the list `args'.

`function' is the function associated with this node. The arguments
passed to `function' are the respective results of the child node
computations.

`ptree' is the ptree instance in which the node is being defined."
  (with-%ptree-slots (nodes) ptree
    (flet ((fetch-node (id) (or (gethash id nodes)
                                (setf (gethash id nodes)
                                      (make-node-instance :id id)))))
      (let1 node (fetch-node id)
        (with-node-slots ((node-function function)
                          (node-children children)) node
          (when node-function
            (error 'ptree-redefinition-error :id id))
          (setf node-function function)
          (let1 children (mapcar #'fetch-node args)
            (dolist (child children)
              (with-node-slots (parents) child
                (push node parents)))
            (setf node-children children))))))
  id)

(defun call-ptree (id ptree)
  "Return the computation result of the node with identifier `id' in
`ptree'.

If the node is uncomputed, compute the result.

If the node is already computed, return the computed result."
  (let1 root (gethash id (nodes ptree))
    (unless root
      (error 'ptree-undefined-function-error :id id))
    (unwrap-result
     (result
      (cond ((computedp root)
             root)
            (t
             (wait-for-compute ptree)
             (cond ((computedp root)
                    root)
                   (t
                    (check-kernel)
                    (compute-ptree root ptree *kernel*)))))))))

(defun has-lambda-list-keyword-p (list)
  (some (lambda (elem) (find elem lambda-list-keywords)) list))

(defmacro ptree (defs &body body)
  "Create a ptree using `flet' syntax. 

  ptree ((node-name child-names function-body)*) form*

Each `node-name' form corresponds to the definition of a ptree node.

`node-name' is the name of the node being defined (a symbol).

`child-names' is a list of the names of child nodes (symbols).

The function associated with the node being defined is

  `(lambda ,child-names ,@function-body)

`child-names' cannot contain lambda list keywords.

For each `node-name', a symbol macro is defined which expands to a
`call-ptree' form for that node."
  (dolist (def defs)
    (destructuring-bind (id args &rest forms) def
      (declare (ignore forms))
      (check-type id symbol)
      (check-type args list)
      (when-let (keyword (has-lambda-list-keyword-p args))
        (error 'ptree-lambda-list-keyword-error
               :id id
               :keyword keyword))))
  (with-gensyms (tree)
    `(let1 ,tree (make-ptree)
       ,@(loop
            :for def :in defs
            :collect (destructuring-bind (id args &rest forms) def
                       `(ptree-fn
                         ',id ',args (lambda ,args ,@forms) ,tree)))
       (symbol-macrolet ,(loop
                            :for (id nil nil) :in defs
                            :collect `(,id (call-ptree ',id ,tree)))
         ,@body))))
