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

;;; 
;;; Assuming that `sort' returns the passed vector is technically
;;; nonconforming since the hyperspec says:
;;;
;;; "If sequence is a vector, the result might or might not be simple,
;;; and might or might not be identical to sequence."
;;; 
;;; However I know of no implementation that returns a new vector. The
;;; benefits of calling the built-in `sort' outweigh the (probably
;;; remote) possibility of future incompatibility with some CL
;;; implementation.
;;; 

(defmacro define-dispatch-quicksort (name has-key-p)
  (let* ((key      (when has-key-p (with-gensyms (key) key)))
         (key-args (when has-key-p `(:key ,key))))
    `(defun/ftype ,name
         (quicksort vec lo hi compare min max submit ,@(unsplice key))
         (function
          (function vector fixnum fixnum function fixnum fixnum function
           ,@(unsplice (when key `function)))
          t)
       (declare #.*full-optimize*)
       (when (> hi lo)
         (let1 size (the fixnum (1+ (the fixnum (- hi lo))))
           (declare (type fixnum size))
           (macrolet ((subvec () `(make-displaced-array vec lo size)))
             (cond ((< size min)
                    ;; too small -- compute now
                    (sort (subvec) compare ,@key-args))
                   ((<= size max)
                    ;; goldilocks zone -- call built-in sort
                    (funcall submit 'sort (subvec) compare ,@key-args))
                   (t
                    ;; too big -- recurse
                    (funcall submit quicksort vec lo hi compare min max submit 
                     ,@(unsplice key))))))))))

(define-dispatch-quicksort dispatch-quicksort/key t)
(define-dispatch-quicksort dispatch-quicksort/no-key nil)

(defun/inline midpoint (a b)
  (+ a (ash (- b a) -1)))

;;; 
;;; Adapted from Roger Corman's usenet post. Free license.
;;; 
(defmacro define-quicksort-fn (name key key-type call-key)
  (let1 dispatch (if key 'dispatch-quicksort/key 'dispatch-quicksort/no-key)
    `(defun/ftype ,name (vec lo hi compare min max submit ,@(unsplice key))
         (function (vector fixnum fixnum function fixnum fixnum function
                    ,@(unsplice key-type))
                   null)
       (declare #.*full-optimize*)
       (when (> hi lo)
         (let* ((mid (the fixnum (midpoint lo hi)))
                (i lo)
                (j (the fixnum (1+ hi)))
                (p (,call-key (aref vec mid))))
           (declare (type fixnum mid i j))
           (rotatef (aref vec mid)
                    (aref vec lo))
           (loop
              (loop
                 :do (incf i)
                 :until (or (> i hi)
                            (funcall compare p (,call-key (aref vec i)))))
              (loop
                 :do (decf j)
                 :until (or (<= j lo)
                            (funcall compare (,call-key (aref vec j)) p)))
              (when (< j i)
                (return))
              (rotatef (aref vec i)
                       (aref vec j)))
           (rotatef (aref vec lo)
                    (aref vec j))
           (,dispatch #',name vec lo (the fixnum (1- j)) 
                      compare min max submit ,@(unsplice key))
           (,dispatch #',name vec i  hi
                      compare min max submit ,@(unsplice key))))
       nil)))

(defmacro define-quicksort/no-key ()
  (with-gensyms (call-key)
    `(macrolet ((,call-key (x) x))
       (define-quicksort-fn quicksort/no-key nil nil ,call-key))))

(defmacro define-quicksort/key ()
  (with-gensyms (call-key key)
    `(macrolet ((,call-key (x) `(funcall ,',key ,x)))
       (define-quicksort-fn quicksort/key ,key function ,call-key))))

(define-quicksort/key)
(define-quicksort/no-key)

(defun call-quicksort (vec lo hi compare min max submit key)
  (if key
      (quicksort/key    vec lo hi compare min max submit (ensure-function key))
      (quicksort/no-key vec lo hi compare min max submit)))

(defun psort (sequence predicate &key key parts min-part-size max-part-size)
  "Parallel version of `sort'.

`sequence' is sorted recursively in parts in parallel. A part is
sorted in parallel if its size is between `min-part-size' and
`max-part-size'. Smaller parts are sorted immediately; larger parts
are recursed upon.

`min-part-size' defaults to some small number. 

`max-part-size' defaults to (/ (length sequence) parts). 

`parts' defaults to (kernel-worker-count)."
  (when min-part-size (check-type min-part-size fixnum))
  (when max-part-size (check-type max-part-size fixnum))
  (typecase sequence
    (vector
     (let* ((predicate  (ensure-function predicate))
            (parts-hint (get-parts-hint parts))
            (min        (or min-part-size 2))
            (max        (or max-part-size (floor (length sequence) parts-hint)))
            (last       (1- (length sequence))))
       (with-submit-dynamic-counted
         (call-quicksort
          sequence 0 last predicate min max #'submit-dynamic-counted key)
         (receive-dynamic-counted))
       sequence))
    (otherwise
     (sort sequence predicate :key key))))
