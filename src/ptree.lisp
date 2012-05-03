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

#.(import 'lparallel.kernel::*kernel-thread-locals*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; errors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition ptree-error (error)
  ((name :initarg :name :reader ptree-error-name)))

(define-condition ptree-redefinition-error (ptree-error)
  ()
  (:report
   (lambda (err stream)
     (format stream "ptree function redefined: ~a" (ptree-error-name err)))))

(define-condition ptree-undefined-function-error (ptree-error)
  ((refs :initarg :refs :reader ptree-error-refs))
  (:report
   (lambda (err stream)
     (format stream
             "Function not found in ptree: ~a~%Referenced by: ~{~a ~}"
             (ptree-error-name err)
             (ptree-error-refs err)))))

(define-condition ptree-lambda-list-keyword-error (ptree-error)
  ((keyword :initarg :keyword :reader ptree-error-keyword))
  (:report
   (lambda (err stream)
     (format stream
             "Function arguments in PTREE cannot contain lambda list ~
              keywords:~%Found ~a in the definition of ~a"
             (ptree-error-keyword err)
             (ptree-error-name err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; node
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defslots node ()
  ((name             :reader name                     :type symbol)
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
  (with-node-slots (name function parents) node
    (unless function
      (error 'ptree-undefined-function-error
             :name name
             :refs (mapcar #'name parents)))))

(defun/ftype compute-node (node) (function (node) t)
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

(defun/inline freep (node)
  (zerop (lock-level node)))

(defun/ftype lock-node (node) (function (node) null)
  (declare #.*full-optimize*)
  (with-node-slots (lock-level parents) node
    (incf lock-level)
    (dolist (parent parents)
      (lock-node parent))))

(defun/ftype unlock-node (node) (function (node) null)
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

(defun/ftype submit-node (channel node) (function (channel node) null)
  (declare #.*full-optimize*)
  (if *ptree-node-kernel*
      (submit-task channel (let1 node-kernel *ptree-node-kernel*
                             (lambda ()
                               (let1 *kernel* node-kernel
                                 (compute-node node)))))
      (submit-task channel 'compute-node node))
  nil)

(defun/ftype find-children-results (node) (function (node) t)
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

(defun/ftype find-node (node) (function (node) (or node null))
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

(defun/ftype master-loop (root) (function (node) node)
  (declare #.*full-optimize*)
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

(defslots ptree ()
  ((nodes :initform (make-hash-table :test #'eq) :type hash-table)))

(defun check-ptree (ptree)
  "Verify that all nodes have been defined with an associated
function. If not, `ptree-undefined-function-error' is signaled."
  (with-ptree-slots (nodes) ptree
    (maphash (lambda (name node)
               (declare (ignore name))
               (check-node node))
             nodes)))

(defun make-ptree ()
  "Create a ptree instance."
  (make-ptree-instance))

(defun ptree-fn (name args function ptree)
  "Define a ptree node labeled `name' (a symbol) in `ptree' (a ptree instance).

The names of its child nodes are the symbols in `args'.

`function' is the function associated with this node. The arguments
passed to `function' are the respective results of the child node
computations."
  (with-ptree-slots (nodes) ptree
    (flet ((fetch (name) (or (gethash name nodes)
                             (setf (gethash name nodes)
                                   (make-node-instance :name name)))))
      (let1 node (fetch name)
        (with-node-slots ((node-function function)
                          (node-children children)) node
          (when node-function
            (error 'ptree-redefinition-error :name name))
          (setf node-function function)
          (let1 children (mapcar #'fetch args)
            (dolist (child children)
              (with-node-slots (parents) child
                (push node parents)))
            (setf node-children children))))))
  name)

(defun call-ptree (name ptree)
  "Return the computation result of the node labeled `name' (a symbol)
in `ptree' (a ptree instance).

If the node is uncomputed, compute the result.

If the node is already computed, return the computed result. A node is
never computed twice."
  (with-ptree-slots (nodes) ptree
    (let1 root (gethash name nodes)
      (unless root
        (error 'ptree-undefined-function-error :name name))
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
      (destructuring-bind (name args &rest forms) def
        (declare (ignore forms))
        (check-type name symbol)
        (check-type args list)
        (when-let (keyword (has-lambda-list-keyword-p args))
          (error 'ptree-lambda-list-keyword-error
                 :name name
                 :keyword keyword))))
    (with-gensyms (tree)
      `(let1 ,tree (make-ptree)
         ,@(loop
              :for def :in defs
              :collect (destructuring-bind (name args &rest forms) def
                         `(ptree-fn
                           ',name ',args (lambda ,args ,@forms) ,tree)))
         (symbol-macrolet ,(loop
                              :for (name nil nil) :in defs
                              :collect `(,name (call-ptree ',name ,tree)))
           ,@body)))))
