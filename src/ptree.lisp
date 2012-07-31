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

(import-now lparallel.kernel::*kernel-thread-locals*
            lparallel.kernel::channel)

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

(defslots node ()
  ((id               :reader id)
   (function                            :initform nil :type (or function null))
   (children                            :initform nil :type list)
   (parents                             :initform nil :type list)
   (computed         :reader computed   :initform nil :type (or boolean error))
   (lock-level       :reader lock-level :initform 0   :type fixnum)
   (children-results                    :initform nil :type list)
   (result           :reader result     :initform nil)))

(define-thread-locals *kernel-thread-locals*
  (*ptree-node-kernel* nil
   "Thread-local. When non-nil, `*kernel*' is bound to this value
    during the call of a node function."))

(defun check-node (node)
  (with-node-slots (id function parents) node
    (unless function
      (error 'ptree-undefined-function-error
             :id id
             :refs (mapcar #'id parents)))))

(defun/type compute-node (node) (node) node
  (declare #.*normal-optimize*)
  (with-node-slots (function computed children-results result) node
    (handler-case
        (progn
          (unless function
            (check-node node))
          (setf result (apply (the function function) children-results)
                computed t))
      (error (err) (setf computed err))))
  node)

(defun/type/inline freep (node) (node) t
  (zerop (lock-level node)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; master loop
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun/type submit-node (channel node) (channel node) null
  (declare #.*normal-optimize*)
  (if *ptree-node-kernel*
      (submit-task channel (let1 node-kernel *ptree-node-kernel*
                             (lambda ()
                               (let1 *kernel* node-kernel
                                 (compute-node node)))))
      (submit-task channel #'compute-node node))
  nil)

(defun/type find-children-results (node) (node) (or list boolean)
  "For non-nil children, sets children-results upon first success."
  (declare #.*full-optimize*)
  (with-node-slots (children children-results) node
    (or (null children)               ; vacuously true for no children
        children-results
        (setf children-results (progn
                                 (dolist (child children)
                                   (unless (computed child)
                                     (return-from find-children-results nil)))
                                 (mapcar #'result children))))))

(defun/type find-node (node) (node) (or node null)
  (declare #.*full-optimize*)
  (with-node-slots (computed children) node
    (cond (computed
           ;;
           ;; already computed
           ;;
           nil)
          ((and (freep node) (find-children-results node))
           ;;
           ;; Node is not computed, not locked, and its children are
           ;; computed; ready to compute.
           ;;
           node)
          (t
           ;;
           ;; computed or locked or children not computed; recurse to children
           ;;
           (dolist (child children)
             (when-let (found (find-node child))
               (return found)))))))

(defun/type master-loop (root) (node) node
  (declare #.*normal-optimize*)
  (let1 channel (make-channel)
    (loop (let1 node (find-node root)
            (cond (node
                   (lock-node node)
                   (submit-node channel node))
                  (t
                   (setf node (receive-result channel))
                   (unlock-node node)
                   (with-node-slots (computed) node
                     (when (or (eq node root) (not (eq computed t)))
                       (return node)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ptree
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots %ptree ()
  ((nodes :initform (make-hash-table :test #'eq) :type hash-table)))

(defun check-ptree (ptree)
  "Verify that all nodes have been defined with an associated
function. If not, `ptree-undefined-function-error' is signaled."
  (with-%ptree-slots (nodes) ptree
    (maphash (lambda (id node)
               (declare (ignore id))
               (check-node node))
             nodes)))

(defun make-ptree ()
  "Create a ptree instance."
  (make-%ptree-instance))

(defun ptree-fn (id args function ptree)
  "Define a ptree node with identifier `id', which is some unique
object suitable for `eq' comparison, such as symbol.

The ids of its child nodes are elements of the list `args'.

`function' is the function associated with this node. The arguments
passed to `function' are the respective results of the child node
computations.

`ptree' is the ptree instance in which the node is being defined."
  (with-%ptree-slots (nodes) ptree
    (flet ((fetch (id) (or (gethash id nodes)
                           (setf (gethash id nodes)
                                 (make-node-instance :id id)))))
      (let1 node (fetch id)
        (with-node-slots ((node-function function)
                          (node-children children)) node
          (when node-function
            (error 'ptree-redefinition-error :id id))
          (setf node-function function)
          (let1 children (mapcar #'fetch args)
            (dolist (child children)
              (with-node-slots (parents) child
                (push node parents)))
            (setf node-children children))))))
  id)

(defun call-ptree (id ptree)
  "Return the computation result of the node with identifier `id' in
`ptree'.

If the node is uncomputed, compute the result.

If the node is already computed, return the computed result. A node is
never computed twice."
  (with-%ptree-slots (nodes) ptree
    (let1 root (gethash id nodes)
      (unless root
        (error 'ptree-undefined-function-error :id id))
      (let1 node (if (computed root)
                     root
                     (master-loop root))
        (with-node-slots (computed result) node
          (unless (eq t computed)
            (error computed))
          result)))))

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
  (flet ((has-lambda-list-keyword-p (list)
           (some (lambda (elem) (find elem lambda-list-keywords)) list)))
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
           ,@body)))))
