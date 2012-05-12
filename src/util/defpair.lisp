(in-package #:lparallel.util)

(defmacro defpair (name supers (a b) &optional doc)
  "Define a cons type using defclass syntax.

Exactly two slots and zero superclasses must be given.

Available slot options are: `:initform', `:type', `:reader'.

A deftype for `name' is defined.

`(defpair foo ...)' defines the function `make-foo-instance' which
takes keyword arguments corresponding to slots of the same name.

All slots must be initialized when an instance is created, else an
error will be signaled."
  (unless (null supers)
    (error "Non-empty superclass list ~a in DEFPAIR ~a" supers name))
  (when doc
    (unless (and (consp doc) (eq :documentation (car doc)))
      (error "Expected `:documentation' option in DEFPAIR, got ~a" doc)))
  (setf a (mklist a))
  (setf b (mklist b))
  (labels ((slot-name     (slot) (car slot))
           (slot-props    (slot) (cdr slot))
           (slot-initform (slot) (or (getf (slot-props slot) :initform)
                                     `(error "slot ~a in ~a not initialized"
                                             ',(slot-name slot) ',name)))
           (slot-initarg  (slot) (intern (symbol-name (slot-name slot))
                                         'keyword))
           (slot-type     (slot) (or (getf (slot-props slot) :type) t)))
    (when (eq (slot-name a) (slot-name b))
      (error "Multiple slots named ~a in DEFPAIR ~a" (slot-name a) name))
    (dolist (slot (list a b))
      (unless (slot-name slot)
        (error "empty slot in ~a" name))
      (when (slot-props slot)
        (let1 diff (set-difference (plist-keys (slot-props slot))
                                   '(:initform :type :reader))
          (unless (null diff)
            (error "Invalid slot option~p in DEFPAIR: ~{~a^ ~}"
                   (length diff) diff)))))
    `(progn
       (deftype ,name () `(cons ,',(slot-type a)
                                ,',(slot-type b)))

       (defun/type/inline ,(intern-conc '#:make- name '#:-instance)
           (&key (,(slot-name a) ,(slot-initform a))
                 (,(slot-name b) ,(slot-initform b)))
           (&key (,(slot-initarg a) ,(slot-type a))
                 (,(slot-initarg b) ,(slot-type b)))
           ,name
         (cons ,(slot-name a) ,(slot-name b)))

       ,@(loop
            :for slot :in `(,a ,b)
            :for fn   :in '(car cdr)
            :for readers := (plist-values-for-key (slot-props slot) :reader)
            :when readers
            :collect `(progn
                        ,@(loop
                             :for reader :in readers
                             :collect `(alias-function ,reader ,fn)))))))
