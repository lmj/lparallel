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

(in-package #:lparallel.cognate)

(defun/type/inline midpoint (a b) (fixnum fixnum) fixnum
  (declare #.*full-optimize*)
  (+ a (the fixnum (ash (the fixnum (- b a)) -1))))

;;;
;;; Adapted from Roger Corman's usenet post. Free license.
;;;
(defmacro define-quicksort-fn (name call-key key key-type gran gran-type)
  `(defpun/type ,name (vec lo hi compare ,@(unsplice gran) ,@(unsplice key))
       (vector fixnum fixnum function
        ,@(unsplice gran-type) ,@(unsplice key-type))
       (values)
     (declare #.*full-optimize*)
     #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     (when (> hi lo)
       (let* ((mid (the fixnum (midpoint lo hi)))
              (i lo)
              (j (the fixnum (1+ hi)))
              (p (,call-key (aref vec mid))))
         (declare (type fixnum mid i j))
         (rotatef (aref vec mid)
                  (aref vec lo))
         (loop
            (loop do (incf i)
                  until (or (> i hi)
                            (funcall compare p (,call-key (aref vec i)))))
            (loop do (decf j)
                  until (or (<= j lo)
                            (funcall compare (,call-key (aref vec j)) p)))
            (when (< j i)
              (return))
            (rotatef (aref vec i)
                     (aref vec j)))
         (rotatef (aref vec lo)
                  (aref vec j))
         ,(let ((left  `(,name vec lo (the fixnum (1- j))
                               compare ,@(unsplice gran) ,@(unsplice key)))
                (right `(,name vec i hi
                               compare ,@(unsplice gran) ,@(unsplice key))))
            (if gran
                `(let ((left-size (the fixnum (- j lo))))
                   (declare (type fixnum left-size))
                   (if (> left-size ,gran)
                       (plet ((left-result ,left)
                              (right-result ,right))
                         (declare (ignore left-result right-result)))
                       (let ((right-size (the fixnum
                                           (1+ (the fixnum (- hi i))))))
                         (declare (type fixnum right-size))
                         (if (> right-size ,gran)
                             (plet ((right-result ,right)
                                    (left-result ,left))
                               (declare (ignore left-result right-result)))
                             (cond ((< left-size right-size)
                                    ,left
                                    ,right)
                                   (t
                                    ,right
                                    ,left))))))
                `(plet ((right-result ,right)
                        (left-result ,left))
                   (declare (ignore right-result left-result)))))))
     (values)))

(defmacro define-quicksort-fns ()
  (with-gensyms (iden call-key key gran)
    `(macrolet ((,iden (x) x)
                (,call-key (x) `(funcall ,',key ,x)))
       (define-quicksort-fn quicksort/no-key/no-gran
           ,iden nil nil nil nil)
       (define-quicksort-fn quicksort/no-key/gran
           ,iden nil nil ,gran fixnum)
       (define-quicksort-fn quicksort/key/no-gran
           ,call-key ,key function nil nil)
       (define-quicksort-fn quicksort/key/gran
           ,call-key ,key function ,gran fixnum))))

(define-quicksort-fns)

;;; reduce some clutter in defpun expansions; it's safe to remove
;;; these because users should not call them directly
(lparallel.defpun::delete-registered-names
 '(quicksort/no-key/no-gran
   quicksort/no-key/gran
   quicksort/key/no-gran
   quicksort/key/gran))

(defun call-quicksort (vec lo hi compare granularity key)
  (if key
      (if granularity
          (quicksort/key/gran       vec lo hi compare granularity key)
          (quicksort/key/no-gran    vec lo hi compare             key))
      (if granularity
          (quicksort/no-key/gran    vec lo hi compare granularity)
          (quicksort/no-key/no-gran vec lo hi compare))))

(defun psort (sequence predicate &key key granularity &allow-other-keys)
  (typecase sequence
    (vector
     (when granularity
       (check-type granularity fixnum))
     (call-quicksort sequence
                     0
                     (1- (length sequence))
                     (ensure-function predicate)
                     granularity
                     (and key (ensure-function key)))
     sequence)
    (otherwise
     (sort sequence predicate :key key))))

(setf (documentation 'psort 'function)
"Parallel version of `sort'.

If `granularity' is provided then parallel tasks are created only for
segments larger than `granularity'. This may or may not result in
better performance.

At present `psort' is only parallelized for vectors; other types are
given to `cl:sort'.")

(defun psort* (&rest args)
  "Deprecated. Instead use `psort' and pass `:use-caller t' to
`make-kernel'."
  (apply #'psort args))

(define-compiler-macro psort* (&whole whole &rest args)
  (declare (ignore args))
  (simple-style-warning
   "`psort*' is deprecated. Instead use `psort' and pass ~
    `:use-caller t' to `make-kernel'.")
  whole)
