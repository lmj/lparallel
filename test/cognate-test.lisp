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

(in-package #:lparallel-test)

(lp-test pmap-into-test
  (let1 a (list nil nil nil)
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equal '(15 17 19) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equal '(15 17 19) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equal '(15 17 19) a)))
  (let1 a (list nil)
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equal '(15) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equal '(15) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equal '(15) a)))
  (let1 a (vector nil nil nil)
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equalp #(15 17 19) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equalp #(15 17 19) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equalp #(15 17 19) a)))
  (let1 a (vector nil)
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equalp #(15) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equalp #(15) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equalp #(15) a))))

#+sbcl
(lp-base-test map-into-test
  (let1 a (list nil nil nil)
    (lparallel.cognate::map-into a '+ '(5 6 7) '(10 11 12))
    (is (equal '(15 17 19) a)))
  (let1 a (list nil)
    (lparallel.cognate::map-into a '+ '(5 6 7) '(10 11 12))
    (is (equal '(15) a)))
  (let1 a (vector nil nil nil)
    (lparallel.cognate::map-into a '+ '(5 6 7) '(10 11 12))
    (is (equalp #(15 17 19) a)))
  (let1 a (vector nil)
    (lparallel.cognate::map-into a '+ '(5 6 7) '(10 11 12))
    (is (equalp #(15) a)))
  (let1 a (vector 9 9 9)
    (lparallel.cognate::map-into a 'identity #(3 4 5))
    (is (equalp #(3 4 5) a)))
  (let1 a (make-array 5 :fill-pointer 5 :initial-contents '(1 2 3 4 5))
    (map-into a 'identity #(9 9))
    (is (= 2 (length a)))
    (is (equalp #(9 9) a))))

#+sbcl
(progn
  (defclass my-seq (sequence) ())
  (defmethod sb-sequence:length ((s my-seq))
    2)
  (defmethod sb-sequence:elt ((s my-seq) index)
    (case index
      (0 77)
      (1 99)))
  (defmethod sb-sequence:iterator-step ((s my-seq) iterator from-end)
    (if from-end
        (1- iterator)
        (1+ iterator)))
  (defmethod sb-sequence:iterator-endp ((s my-seq) iterator limit from-end)
    (= iterator limit))
  (defmethod sb-sequence:iterator-element ((s my-seq) iterator)
    (case iterator
      (0 77)
      (1 99)))
  (defmethod (setf sb-sequence:iterator-element) (o (s my-seq) iterator)
    (declare (ignore o s iterator)))
  (defmethod sb-sequence:iterator-index ((s my-seq) iterator)
    iterator)
  (defmethod sb-sequence:iterator-copy ((s my-seq) iterator)
    iterator)
  (lp-base-test generic-map-into-test
    (is (equalp '(77 99)
                (map 'list 'identity
                     (lparallel.cognate::map-into
                      (make-instance 'my-seq) 'identity #(0 0)))))))

(lp-test degenerate-pmaps-test
  (is (eq (map  nil #'identity '(0 1 2 3))
          (pmap nil #'identity '(0 1 2 3))))
  (is (eq (map  nil 'identity '(0 1 2 3))
          (pmap nil 'identity '(0 1 2 3))))
  (is (eq (map-into  nil '+ '(2 3) '(4 5))
          (pmap-into nil '+ '(2 3) '(4 5))))
  (is (equalp (map  'vector #'identity '(0 1 2 3))
              (pmap 'vector #'identity '(0 1 2 3))))
  (is (equalp (map  'vector 'identity '(0 1 2 3))
              (pmap 'vector 'identity '(0 1 2 3))))
  (is (equalp (map-into  nil '+ '(2 3) '(4 5))
              (pmap-into nil '+ '(2 3) '(4 5))))
  (is (equal (mapc  #'identity '(0 1 2 3))
             (pmapc #'identity '(0 1 2 3))))
  (is (equal (mapc  'identity '(0 1 2 3))
             (pmapc 'identity '(0 1 2 3))))
  (is (equal (mapc  #'identity '(0 1 2 3))
             (pmapc #'identity :parts 4 '(0 1 2 3))))
  (is (equal (mapc  'identity '(0 1 2 3))
             (pmapc 'identity :parts 4 '(0 1 2 3))))
  (is (equal (mapl  #'identity '(0 1 2 3))
             (pmapl #'identity '(0 1 2 3))))
  (is (equal (mapl  'identity '(0 1 2 3))
             (pmapl 'identity '(0 1 2 3))))
  (is (equal (mapl  #'identity '(0 1 2 3))
             (pmapl #'identity :parts 4 '(0 1 2 3))))
  (is (equal (mapl  'identity '(0 1 2 3))
             (pmapl 'identity :parts 4 '(0 1 2 3)))))

(lp-test pmap-nil-test
  (loop
     :for n :in '(0 1 2 3 4 5 6 7 8 9 10 100 1000)
     :do (let ((a (loop :for x :from 0 :repeat n :collect x))
               (b (loop :for x :from 0 :repeat n :collect (* 2 x)))
               (q (make-queue)))
           (pmap nil (lambda (x y) (push-queue (+ x y) q)) a b)
           (is (equal (sort (extract-queue q) #'<)
                      (loop :for x :from 0 :repeat n :collect (* 3 x)))))))

(lp-test pmapcar-test
  (is (equal '(15 17 19)
             (pmapcar '+ '(5 6 7) '(10 11 12))))
  (is (equal '(15 17 19)
             (pmapcar '+ :parts 3 '(5 6 7) '(10 11 12)))))

(lp-test pmapcar-handles-sequences-test
  (is (equal (mapcar  '+ '(1 2 3) '(4 5 6))
             (pmapcar '+ '(1 2 3) #(4 5 6))))
  (is (equal (mapcar  '+ '(1 2 3) '(4 5 6))
             (pmapcar '+ :parts 3 '(1 2 3) #(4 5 6)))))

(lp-test grind-pmap-test
  (flet ((f (x y z)
           (* x y z)))
    (let* ((lists (collect-n 3
                    (collect-n 500
                      (random 1000))))
           (args (cons #'f lists))
           (expected (apply #'mapcar args)))
      (is (equal expected (apply #'pmapcar args)))
      (is (equal expected (apply #'pmapcar #'f :parts 500 lists)))
      (is (equalp (map 'simple-vector #'identity expected)
                  (apply #'pmap 'simple-vector args)))
      (is (equalp (map 'simple-vector #'identity expected)
                  (apply #'pmap 'simple-vector #'f :parts 500 lists)))
      (let1 result (make-list 500)
        (is (equal expected (apply #'pmap-into result args)))
        (is (equal expected result)))
      (let1 result (make-list 500)
        (is (equal expected (apply #'pmap-into result #'f :parts 500 lists)))
        (is (equal expected result)))
      (dolist (parts '(nil 1000))
        (setf *memo* (make-queue))
        (apply #'pmapc
               (lambda (i x y z)
                 (push-queue (cons i (f x y z)) *memo*))
               :parts parts
               (loop
                  :for i :from 0 :below (apply #'min (mapcar #'length lists))
                  :collect i)
               lists)
        (is (= (length expected)
               (queue-count *memo*)))
        (is (equal
             expected
             (map 'list #'cdr (sort (extract-queue *memo*) '< :key #'car)))))
      (flet ((join (x y z) (list x y z)))
        (is (equal (apply #'mapcan  #'join lists)
                   (apply #'pmapcan #'join lists)))
        (is (equal (apply #'mapcon  #'join lists)
                   (apply #'pmapcon #'join lists))))
      (is (equal (mapcan  'list (list 3 4 5 6 7 8))
                 (pmapcan 'list (list 3 4 5 6 7 8))))
      (is (equal (mapcon  (lambda (x) (list (car x))) (list 3 4 5 6 7 8))
                 (pmapcon (lambda (x) (list (car x))) (list 3 4 5 6 7 8)))))))

(lp-test pmaplist-test
  (is (equalp (maplist  #'vector '(a b c) '(1 2 3))
              (pmaplist #'vector '(a b c) '(1 2 3)))))

(lp-test grind-pmaplist-test
  (let* ((lists (collect-n 2 (collect-n 100 (random 100))))
         (expected (apply #'maplist #'vector lists)))
    (is (equalp expected
                (apply #'pmaplist #'vector lists)))
    (is (equalp expected
                (apply #'pmaplist #'vector :parts 100 lists)))
    (setf *memo* (make-queue))
    (apply #'pmapl
           (lambda (i x y)
             (push-queue (list i (vector x y)) *memo*))
           (loop
              :for i :from 0 :below (apply #'min (mapcar #'length lists))
              :collect i)
           lists)
    (is (equalp expected
                (map 'list
                     #'cadr
                     (sort (extract-queue *memo*) '< :key #'caar))))))

(lp-test preduce-partial-test
  (signals simple-error
    (preduce-partial #'+ #() :initial-value 0))
  (signals simple-error
    (preduce-partial #'+ '() :initial-value 0))
  (signals simple-error
    (preduce-partial #'+ '()))
  (is (equalp (preduce-partial #'+ '(3 4 5 6 7 8 9 10) :parts 1)
              #(52)))
  (is (equalp (preduce-partial #'+ '(3 4 5 6 7 8 9 10) :parts 2)
              #(18 34)))
  (is (equalp (preduce-partial #'+ '(3 4 5 6 7 8 9 10) :parts 2 :from-end t)
              #(18 34)))
  (is (equalp (preduce-partial #'+ #(3 4 5 6 7 8) :parts 3 :from-end t)
              #(7 11 15)))
  (is (equalp (preduce-partial #'+ #(3 4 5 6 7 8) :parts 3)
              #(7 11 15))))

(lp-test grind-preduce-test
  (is (= 3
         (reduce  (lambda () 3) nil)
         (preduce (lambda () 3) nil)))
  (is (= 3
         (reduce  (lambda (x) (* x x)) nil :initial-value 3)
         (preduce (lambda (x) (* x x)) nil :initial-value 3)))
  (flet ((non-associative/non-commutative (x y)
           (+ (* 2 x) y))
         (associative/non-commutative (a b)
           (vector (+ (* (aref a 0) (aref b 0)) (* (aref a 1) (aref b 2)))
                   (+ (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 3)))
                   (+ (* (aref a 2) (aref b 0)) (* (aref a 3) (aref b 2)))
                   (+ (* (aref a 2) (aref b 1)) (* (aref a 3) (aref b 3)))))
         (verify (test &rest args)
           (loop :for parts :from 1 :to 10 :do
              (is (funcall test
                           (apply #'reduce args)
                           (apply #'preduce args)))
              (is (funcall test
                           (apply #'reduce args)
                           (apply #'preduce
                                  (append args
                                          (list :parts parts)))))
              (is (funcall test
                           (apply #'reduce args)
                           (apply #'preduce
                                  (append args
                                          (list :from-end t)))))
              (is (funcall test
                           (apply #'reduce args)
                           (apply #'preduce
                                  (append args
                                          (list :recurse t)))))
              (is (funcall test
                           (apply #'reduce args)
                           (apply #'preduce
                                  (append args
                                          (list :recurse t :parts parts)))))
              (is (funcall test
                           (apply #'reduce args)
                           (apply #'preduce
                                  (append args
                                          (list :recurse t
                                                :parts parts
                                                :from-end t))))))))
    (let ((a '(0 1 2 3 4 5 6 7))
          (b '((9 . 0) (9 . 1) (9 . 2) (9 . 3)))
          (c (collect-n 100 (random 100)))
          (d (collect-n 100 (vector (random 10)
                                    (random 10)
                                    (random 10)
                                    (random 10)))))
      (verify #'= #'+ a)
      (verify #'= #'+ a :initial-value 0)
      (verify #'= #'+ b :key #'cdr)
      (verify #'= #'+ c)
      (verify #'= #'+ c :initial-value 0)
      (verify #'= #'+ c :start 42)
      (verify #'= #'+ c :end 42)
      (verify #'= #'+ c :start 42 :end 77)
      (verify #'= #'+ c :start 42 :end 77 :from-end t)
      (verify #'= #'+ c :start 42 :end 77 :initial-value 0)
      (verify #'= #'* c :start 42 :end 77 :initial-value 1)
      (verify #'= #'* c :start 42 :end 77 :initial-value 1 :from-end t)

      (verify #'equalp #'associative/non-commutative d)
      (verify #'equalp #'associative/non-commutative d :start 42)
      (verify #'equalp #'associative/non-commutative d :end 42)
      (verify #'equalp #'associative/non-commutative d :start 42 :end 77)
      (verify #'equalp
              #'associative/non-commutative d
              :start 42
              :end 77
              :initial-value (vector 1 0 0 1))
      (verify #'equalp
              #'associative/non-commutative d
              :start 42
              :end 77
              :initial-value (vector 1 0 0 1)
              :from-end t)

      (let1 serial (reduce #'non-associative/non-commutative c)
        (is (= serial
               (preduce #'non-associative/non-commutative c :parts 1)
               (preduce #'non-associative/non-commutative c :parts (length c))))
        (is (/= serial
                (preduce #'non-associative/non-commutative c :parts 3)
                (preduce #'non-associative/non-commutative c :parts 3
                         :from-end t)
                (preduce #'non-associative/non-commutative c :parts 4)
                (preduce #'non-associative/non-commutative c :parts 4
                         :from-end t)
                (preduce #'non-associative/non-commutative c :parts 5))))

      (is (equal (preduce #'+ c :key (lambda (x) (* x x)))
                 (pmap-reduce (lambda (x) (* x x)) #'+ c)))
      (is (equal (+ 9 16 25)
                 (pmap-reduce (lambda (x) (* x x)) #'+ '(3 4 5)))))))

(lp-test grind-pevery-test
  (flet ((verify (&rest args)
           (loop
              :for (regular parallel) :in '((some     psome)
                                            (every    pevery)
                                            (notany   pnotany)
                                            (notevery pnotevery))
              :do (is (eql (apply regular args)
                           (apply parallel args))))))
    (let ((a (collect-n 200 (random 100)))
          (b (collect-n 200 (random 100))))
      (verify (lambda (x) (< x 100)) a)
      (verify (lambda (x) (> x 100)) a)
      (verify (lambda (x) (> x  50)) a)
      (verify (lambda (x) (< x  50)) a)
      (verify (lambda (x) (> x   0)) a)
      (verify (lambda (x) (< x   0)) a)
      (verify (lambda (x y) (< (+ x y) 200)) a b)
      (verify (lambda (x y) (> (+ x y) 200)) a b)
      (verify (lambda (x y) (< (+ x y) 100)) a b)
      (verify (lambda (x y) (> (+ x y) 100)) a b)
      (verify (lambda (x y) (< (+ x y)   0)) a b)
      (verify (lambda (x y) (> (+ x y)   0)) a b))))

(lp-test parts-arg-test
  (flet ((sq (x) (* x x)))
    (loop
       :for parts :from 1 :to 8
       :do (loop
              :for n :from 1 :to 6
              :do (let1 a (collect-n n (random n))
                    (is (equalp ( map-into (make-array n) #'sq a)
                                (pmap-into (make-array n) #'sq :parts parts a)))
                    (is (equal  ( map-into (make-list n) #'sq a)
                                (pmap-into (make-list n) #'sq :parts parts a)))
                    (is (equalp ( map 'vector #'sq a)
                                (pmap 'vector #'sq :parts parts a)))
                    (is (equal  ( map 'list #'sq a)
                                (pmap 'list #'sq :parts parts a)))
                    (is (equal  ( mapcar #'sq a)
                                (pmapcar #'sq :parts parts a)))
                    (is (equal  ( maplist #'car a)
                                (pmaplist #'car :parts parts a)))
                    (is (equal  ( mapcan #'list a)
                                (pmapcan #'list :parts parts a)))
                    (is (equal  ( mapcon #'list a)
                                (pmapcon #'list :parts parts a)))
                    (pmapc #'sq :parts parts a)
                    (pmapl #'cdr :parts parts a))))))

(defmacro define-plet-test (test-name fn-name defun)
  `(progn
     (,defun ,fn-name ()
       (plet ((a 3)
              (b 4))
         (is (= 7 (+ a b))))
       (signals client-error
         (task-handler-bind ((error (lambda (e)
                                      (invoke-restart 'transfer-error e))))
           (plet ((a (error 'client-error)))
             a)))
       (task-handler-bind ((error (lambda (e)
                                    (invoke-restart 'transfer-error e))))
         (handler-bind ((error (lambda (e)
                                 (declare (ignore e))
                                 (invoke-restart 'store-value 4))))
           (setf *memo* (lambda () (error "foo")))
           (plet ((a 3)
                  (b (funcall *memo*)))
             (is (= 7 (+ a b)))))))
     (lp-test ,test-name
       (,fn-name))))

(define-plet-test plet-test plet-test-fn defun)

(lp-base-test plet-if-test
  (setf *memo* 0)
  (plet-if (plusp *memo*)
      ((a 3))
    (is (= 3 a)))
  (signals no-kernel-error
    (plet-if (zerop *memo*)
        ((a 3))
      (is (= 3 a)))))

(lp-test pand-por-test
  (is (null (pand 3 4 5 6 nil)))
  (is (null (pand 3 4 nil 5 6)))
  (is (null (pand nil 3 4 5 6)))
  (is (member (pand 3 4 5 6) '(3 4 5 6)))

  (is (member (por 3 4 5 6 nil) '(3 4 5 6)))
  (is (member (por 3 4 nil 5 6) '(3 4 5 6)))
  (is (member (por nil 3 4 5 6) '(3 4 5 6)))

  (when (> (kernel-worker-count) 2)
    (sleep 0.4)
    (is (= 4 (por  (progn (sleep 0.2) 3) 4)))
    (sleep 0.4)
    (is (= 3 (pand (progn (sleep 0.2) 3) 4)))

    (sleep 0.4)
    (is (= 4 (por  nil (progn (sleep 0.2) 3) 4)))
    (sleep 0.4)
    (is (= 4 (por  (progn (sleep 0.2) 3) nil 4)))

    (sleep 0.4)
    (is (null (pand nil (progn (sleep 0.2) 3) 4)))
    (sleep 0.4)
    (is (null (pand (progn (sleep 0.2) 3) nil 4)))))

(lp-test psort-test
  ;; psort requires the CL implementation to sort vectors in place.
  (dolist (size '(0 1 2 3 4 8 16 100))
    (let1 v (make-random-seq 'vector size)
      (is (eq v (sort v #'<)))))

  ;; abcl workarounds for worse-case sort bug
  (let1 source (make-random-vector 10000)
    (let ((a (copy-seq source))
          (b (copy-seq source)))
      (is (equalp ( sort a #'<)
                  (psort b #'<)))
      #-abcl
      (is (equalp ( sort a #'<)
                  (psort b #'<)))
      #-abcl
      (is (equalp ( sort a #'>)
                  (psort b #'>)))
      #-abcl
      (is (equalp ( sort a #'>)
                  (psort b #'>))))
    (let ((a (copy-seq source))
          (b (copy-seq source)))
      (is (equalp ( sort a #'<)
                  (psort b
                         #'<
                         :min-part-size 100
                         :max-part-size 1000)))
      #-abcl
      (is (equalp ( sort a #'<)
                  (psort b
                         #'<
                         :min-part-size 100
                         :max-part-size 1000)))
      #-abcl
      (is (equalp ( sort a #'>)
                  (psort b
                         #'>
                         :min-part-size 100
                         :max-part-size 1000)))
      #-abcl
      (is (equalp ( sort a #'>)
                  (psort b
                         #'>
                         :min-part-size 100
                         :max-part-size 1000)))))
  (let1 source (vector 5 1 9 3 6 0 1 9)
    (dolist (i (loop :for i :from 1 :upto 8 :collect i))
      (let ((a (copy-seq source))
            (b (copy-seq source)))
        (is (equalp ( sort a #'<)
                    (psort b #'< :parts i)))
        #-abcl
        (is (equalp ( sort a #'<)
                    (psort b #'< :parts i)))
        #-abcl
        (is (equalp ( sort a #'>)
                    (psort b #'> :parts i)))
        #-abcl
        (is (equalp ( sort a #'>)
                    (psort b #'> :parts i))))))
  (let1 source (vector 5 1 9 3 6 0 1 9)
    (dolist (i (loop :for i :from 1 :upto 8 :collect i))
      (let ((a (copy-seq source))
            (b (copy-seq source)))
        (is (equalp ( sort a #'< :key (lambda (x) (* -1 x)))
                    (psort b #'< :key (lambda (x) (* -1 x)) :parts i)))
        #-abcl
        (is (equalp ( sort a #'< :key (lambda (x) (* -1 x)))
                    (psort b #'< :key (lambda (x) (* -1 x)) :parts i)))
        #-abcl
        (is (equalp ( sort a #'> :key (lambda (x) (* -1 x)))
                    (psort b #'> :key (lambda (x) (* -1 x)) :parts i)))
        #-abcl
        (is (equalp ( sort a #'> :key (lambda (x) (* -1 x)))
                    (psort b #'> :key (lambda (x) (* -1 x)) :parts i))))))
  (let1 source (make-array 50 :initial-element 5)
    (dolist (i (loop :for i :from 1 :upto 8 :collect i))
      (let ((a (copy-seq source))
            (b (copy-seq source)))
        (is (equalp ( sort a #'<)
                    (psort b #'< :parts i)))
        #-abcl
        (is (equalp ( sort a #'<)
                    (psort b #'< :parts i)))
        #-abcl
        (is (equalp ( sort a #'>)
                    (psort b #'> :parts i)))
        #-abcl
        (is (equalp ( sort a #'>)
                    (psort b #'> :parts i)))))))

(lp-test premove-if-test
  (loop
     :for size :below 100
     :for where := (random 1.0)
     :for source := (collect-n size (random 1.0))
     :do (is (equal (remove-if  (curry #'< where) source)
                    (premove-if (curry #'< where) source)))))

(lp-test second-premove-if-test
  (loop
     :for (std par) :in '((remove-if-not premove-if-not)
                          (remove-if     premove-if))
     :do (loop
            :for size :below 100
            :for where := (random 1.0)
            :for a := (make-random-list size)
            :for b := (make-random-vector size)
            :do (is (equal (funcall std (curry #'< where) a)
                           (funcall par (curry #'< where) a)))
            :do (is (equalp (funcall std (curry #'< where) b)
                            (funcall par (curry #'< where) b)))
            :do (when (>= size 77)
                  (is (equal (funcall std (curry #'< where) a
                                      :start 20)
                             (funcall par (curry #'< where) a
                                      :start 20)))
                  (is (equal (funcall std (curry #'< where) a
                                      :start 20 :end 77)
                             (funcall par (curry #'< where) a
                                      :start 20 :end 77)))))))

(lp-test premove-test
  (loop
     :for size :below 100
     :for where := (random 1.0)
     :for a := (make-random-list size)
     :for b := (make-random-vector size)
     :do (progn
           (is (equal (remove  where a :test #'<)
                      (premove where a :test #'<)))
           (is (equal (remove  where a :test-not #'>=)
                      (premove where a :test-not #'>=)))
           (is (equalp (remove  where b :test #'<)
                       (premove where b :test #'<)))
           (is (equalp (remove  where b :test-not (complement #'<))
                       (premove where b :test-not (complement #'<))))))
  (is (equal (remove  3 (list 0 1 2 3 4 9 3 2 3 9 1))
             (premove 3 (list 0 1 2 3 4 9 3 2 3 9 1))))
  (is (equalp (remove  3 (make-array 11
                                     :adjustable t
                                     :initial-contents
                                     (list 0 1 2 3 4 9 3 2 3 9 1)))
              (premove 3 (make-array 11
                                     :adjustable t
                                     :initial-contents
                                     (list 0 1 2 3 4 9 3 2 3 9 1)))))
  (let1 x (cons nil nil)
    (is (equal (remove  x (list 3 4 x 4 9 x 2))
               (premove x (list 3 4 x 4 9 x 2)))))
  (let1 x (cons nil nil)
    (is (equalp (remove  x (make-array
                            7
                            :adjustable t
                            :initial-contents (list 3 4 x 4 9 x 2)))
                (premove x (make-array
                            7
                            :adjustable t
                            :initial-contents (list 3 4 x 4 9 x 2)))))))

(define-condition foo-warning (warning) ())

(lp-base-test worker-context-test
  (flet ((my-worker-context (fn)
           (handler-bind ((warning (lambda (e)
                                     (declare (ignore e))
                                     (invoke-restart 'double-me 3))))
             (funcall fn))))
    (dolist (n '(1 2 3 4 5 6 10))
      (let1 result (with-new-kernel (n :context #'my-worker-context)
                     (pmapcar (lambda (x)
                                (declare (ignore x))
                                (restart-case (warn 'foo-warning)
                                  (double-me (z) (* 2 z))))
                              '(3 3)))
        (is (equal '(6 6) result))))))

(lp-test cognate-handler-test
  (task-handler-bind ((foo-error (lambda (e)
                                   (declare (ignore e))
                                   (invoke-restart 'something-else 3))))
    (is (equal '(3 3)
               (pmapcar (lambda (x)
                          (declare (ignore x))
                          (restart-case (error 'foo-error)
                            (something-else (z) z)))
                        '(0 1))))))

(lp-test pmap-handler-test
  (task-handler-bind ((foo-error
                       (lambda (e) (invoke-restart 'transfer-error e))))
    (signals foo-error
      (pmapcar (lambda (x)
                 (declare (ignore x))
                 (error 'foo-error))
               '(3 4 5 6)))))

(lp-test pmap-restart-test
  (task-handler-bind
      ((foo-error (lambda (e)
                    (declare (ignore e))
                    (invoke-restart 'thirty-three))))
    (is (equal '(3 3)
               (pmapcar (lambda (x)
                          (declare (ignore x))
                          (restart-case (error 'foo-error)
                            (thirty-three ()
                              3)))
                        '(0 0))))))

(lp-test pmap-into-bounds-test
  (dotimes (i 3)
    (dotimes (j (1+ i))
      (let1 contents (collect-n i (random 1000))
        (destructuring-bind (a b)
            (collect-n 2
              (make-array i :fill-pointer j :initial-contents contents))
          (dotimes (k 6)
            (let1 source (collect-n k (random 1000))
              (let ((c (pmap-into b #'identity source))
                    (d (map-into  a #'identity source)))
                (is (equalp a b))
                (is (equalp c d))))))))))

(lp-test pmap-with-size-constraint-test
  (is (equal '(2 11)
             (pmapcar '1+ :size 2 '(1 10 100 1000))))
  (is (equal '(2 11)
             (pmap 'list '1+ :size 2 '(1 10 100 1000))))
  (is (equalp #(2 11)
              (pmap 'vector '1+ :size 2 '(1 10 100 1000))))
  (is (equalp #(2 11)
              (pmap 'vector '1+ :size 2 #(1 10 100 1000))))
  (is (equalp #(2 11 99 99)
              (pmap-into (vector 99 99 99 99) '1+ :size 2 #(1 10 100 1000))))
  (is (equal '(2 11)
             (pmap-into (list 'a 'b) '1+ :size 2 '(1 10 100 1000))))
  (is (equal '(2 11)
             (pmaplist-into (list 'a 'b)
                            (lambda (x) (1+ (car x)))
                            :size 2
                            '(1 10 100 1000))))
  (is (equal '(2 11 c d)
             (pmap-into (list 'a 'b 'c 'd) '1+ :size 2 '(1 10 100 1000))))
  (is (equal '(2 11 c d)
             (pmaplist-into (list 'a 'b 'c 'd)
                            (lambda (x) (1+ (car x)))
                            :size 2
                            '(1 10 100 1000)))))

(lp-test pmap-into-list-test
  (dotimes (m 10)
    (dotimes (n 10)
      (let* ((src (make-list m :initial-element 'src))
             (dst (make-list n :initial-element 'dst))
             (a (copy-list dst))
             (b (copy-list dst))
             (res-a (map-into  a #'identity src))
             (res-b (pmap-into b #'identity src)))
        (is (eq a res-a))
        (is (eq b res-b))
        (is (equal a b))))))

(lp-test pmap-into-degenerate-input-test
  (is (equalp #()
              (map-into  (vector) (constantly 99))))
  (is (equalp #()
              (pmap-into (vector) (constantly 99))))
  (is (equalp #(99 99)
              (map-into  (vector 1 2) (constantly 99))))
  (is (equalp #(99 99)
              (pmap-into (vector 1 2) (constantly 99))))
  (is (equalp #(1 2 3 4)
              (pmap-into (vector 1 2 3 4) (constantly 99) :size 0)))
  (is (equalp #(99 99 3 4)
              (pmap-into (vector 1 2 3 4) (constantly 99) :size 2)))
  (is (equalp #(99 99 99 99)
              (pmap-into (vector 1 2 3 4) (constantly 99) :size 4)))
  (is (equal '()
             (map-into  (list) (constantly 99))))
  (is (equal '()
             (pmap-into (list) (constantly 99))))
  (is (equal '(99 99)
             (map-into  (list 1 2) (constantly 99))))
  (is (equal '(99 99)
             (pmap-into (list 1 2) (constantly 99))))
  (is (equal '(1 2 3 4)
             (pmap-into (list 1 2 3 4) (constantly 99) :size 0)))
  (is (equal '(99 99 3 4)
             (pmap-into (list 1 2 3 4) (constantly 99) :size 2)))
  (is (equal '(99 99 99 99)
             (pmap-into (list 1 2 3 4) (constantly 99) :size 4))))

(lp-test pmaplist-into-degenerate-input-test
  (is (equal '()
             (pmaplist-into (list) (constantly 99))))
  (is (equal '(99 99)
             (pmaplist-into (list 1 2) (constantly 99))))
  (is (equal '(1 2 3 4)
             (pmaplist-into (list 1 2 3 4) (constantly 99) :size 0)))
  (is (equal '(99 99 3 4)
             (pmaplist-into (list 1 2 3 4) (constantly 99) :size 2)))
  (is (equal '(99 99 99 99)
             (pmaplist-into (list 1 2 3 4) (constantly 99) :size 4))))

(lp-test pfuncall-test
  (is (= 7 (pfuncall '+ 3 4)))
  (let1 memo (make-queue)
    (is (= 7 (pfuncall
              #'+
              (progn (sleep 0.2) (push-queue 3 memo) 3)
              (progn (sleep 0.2) (push-queue 4 memo) 4))))
    (sleep 0.3)
    (is (= 2 (queue-count memo)))))

(lp-test pcount-if-test
  (is (zerop (pcount-if 'non-function '())))
  (is (zerop (pcount-if 'non-function #())))
  (signals error
    (pcount-if 'non-function '() :start 2))
  (loop
     :for size :from 1 :below 100
     :for where := (random 1.0)
     :for source := (collect-n size (random 1.0))
     :do (is (equal (count-if  (curry #'< where) source)
                    (pcount-if (curry #'< where) source)))))

(lp-test second-pcount-if-test
  (loop
     :for (std par) :in '((count-if-not pcount-if-not)
                          (count-if     pcount-if))
     :do (loop
            :for size :from 1 :below 100
            :for where := (random 1.0)
            :for a := (make-random-list size)
            :for b := (make-random-vector size)
            :do (is (equal  (funcall std (curry #'< where) a)
                            (funcall par (curry #'< where) a)))
            :do (is (equalp (funcall std (curry #'< where) b)
                            (funcall par (curry #'< where) b)))
            :do (when (>= size 77)
                  (is (equal (funcall std (curry #'< where) a
                                      :start 20)
                             (funcall par (curry #'< where) a
                                      :start 20)))
                  (is (equal (funcall std (curry #'< where) a
                                      :start 20 :end 77)
                             (funcall par (curry #'< where) a
                                      :start 20 :end 77)))))))

(lp-test pcount-test
  (loop
     :for size :from 1 :below 100
     :for where := (random 1.0)
     :for a := (make-random-list size)
     :for b := (make-random-vector size)
     :do (progn
           (is (equal  (count  where a :test #'<)
                       (pcount where a :test #'<)))
           (is (equal  (count  where a :test-not (complement #'<))
                       (pcount where a :test-not (complement #'<))))
           (is (equalp (count  where b :test #'<)
                       (pcount where b :test #'<)))
           (is (equalp (count  where b :test-not (complement #'<))
                       (pcount where b :test-not (complement #'<))))))
  (is (equal (count  3 (list 0 1 2 3 4 9 3 2 3 9 1))
             (pcount 3 (list 0 1 2 3 4 9 3 2 3 9 1))))
  (is (equalp (count  3 (make-array 11
                                    :adjustable t
                                    :initial-contents
                                    (list 0 1 2 3 4 9 3 2 3 9 1)))
              (pcount 3 (make-array 11
                                     :adjustable t
                                     :initial-contents
                                     (list 0 1 2 3 4 9 3 2 3 9 1)))))
  (let1 x (cons nil nil)
    (is (equal (count  x (list 3 4 x 4 9 x 2))
               (pcount x (list 3 4 x 4 9 x 2)))))
  (let1 x (cons nil nil)
    (is (equalp (count  x (make-array
                           7
                           :adjustable t
                           :initial-contents (list 3 4 x 4 9 x 2)))
                (pcount x (make-array
                           7
                           :adjustable t
                           :initial-contents (list 3 4 x 4 9 x 2)))))))


(lp-test pfind-if-test
  (is (null (pfind-if 'non-function '())))
  (is (null (pfind-if 'non-function #())))
  (signals error (pfind-if 'non-function '() :start 2))
  (signals error (pfind-if 'non-function #() :start 2))
  (is (= 3
         (find-if  (lambda (x) (< x 5)) '(9 9 6 7 3 9 6))
         (pfind-if (lambda (x) (< x 5)) '(9 9 6 7 3 9 6))
         (find-if  (lambda (x) (< x 5)) #(9 9 6 7 3 9 6))
         (pfind-if (lambda (x) (< x 5)) #(9 9 6 7 3 9 6))))
  (is (= 3
         (find-if-not  (lambda (x) (>= x 5)) '(9 9 6 7 3 9 6))
         (pfind-if-not (lambda (x) (>= x 5)) '(9 9 6 7 3 9 6))
         (find-if-not  (lambda (x) (>= x 5)) #(9 9 6 7 3 9 6))
         (pfind-if-not (lambda (x) (>= x 5)) #(9 9 6 7 3 9 6))))
  (loop
     :for size :from 1 :below 100
     :for source := (collect-n size (random 1.0))
     :do (setf (elt source (random size)) 999)
     :do (is (eql (find-if  (curry #'eql 999) source)
                  (pfind-if (curry #'eql 999) source)))))

(lp-test second-pfind-if-test
  (loop
     :for (std par) :in '((find-if pfind-if))
     :do (loop
            :for size :from 1 :below 100
            :for a := (make-random-list size)
            :for b := (make-random-vector size)
            :for target := (let1 index (random size)
                             (setf (elt a index) 99.0
                                   (elt b index) 99.0))
            :do (is (equal  (funcall std (curry #'eql target) a)
                            (funcall par (curry #'eql target) a)))
            :do (is (equalp (funcall std (curry #'eql target) b)
                            (funcall par (curry #'eql target) b)))
            :do (when (>= size 77)
                  (is (equal (funcall std (curry #'eql target) a
                                      :start 20)
                             (funcall par (curry #'eql target) a
                                      :start 20)))
                  (is (equal (funcall std (curry #'eql target) a
                                      :start 20 :end 77)
                             (funcall par (curry #'eql target) a
                                      :start 20 :end 77)))))))

(lp-test pfind-test
  (signals error
    (pfind 3 '(3 3 3) :test #'eql :test-not #'eql))
  (loop
     :for size :from 1 :below 100
     :for a := (make-random-list size)
     :for b := (make-random-vector size)
     :for target := (let1 index (random size)
                      (setf (elt a index) 99.0
                            (elt b index) 99.0))
     :do (is (equal  (find  target a)
                     (pfind target a)))
     :do (is (equalp (find  target b)
                     (pfind target b))))
  (is (equal (find  3 (list 0 1 2 3 4 9 3 2 3 9 1))
             (pfind 3 (list 0 1 2 3 4 9 3 2 3 9 1))))
  (is (equalp (find  3 (make-array 11
                                   :adjustable t
                                   :initial-contents
                                   (list 0 1 2 3 4 9 3 2 3 9 1)))
              (pfind 3 (make-array 11
                                   :adjustable t
                                   :initial-contents
                                   (list 0 1 2 3 4 9 3 2 3 9 1)))))
  (let1 x (cons nil nil)
    (is (equal (find  x (list 3 4 x 4 9 x 2))
               (pfind x (list 3 4 x 4 9 x 2)))))
  (let1 x (cons nil nil)
    (is (equalp (find  x (make-array
                          7
                          :adjustable t
                          :initial-contents (list 3 4 x 4 9 x 2)))
                (pfind x (make-array
                          7
                          :adjustable t
                          :initial-contents (list 3 4 x 4 9 x 2)))))))

(defmacro define-pmap-into-edge-test (name decl)
  `(lp-test ,name
     ,@(unsplice decl)
     (is (equalp #(1 2 3)
                 (pmap-into (vector 9 9 9) 'identity (vector 1 2 3))))
     (is (equalp #(1 2 3)
                 (pmap-into (vector 9 9 9) 'identity :size 3 (vector 1 2 3))))
     (is (equalp #(1 2 9)
                 (pmap-into (vector 9 9 9) 'identity :size 2 (vector 1 2 3))))
     (is (equalp #(9 9 9)
                 (pmap-into (vector 9 9 9) 'identity :size 0 (vector 1 2 3))))
     (is (equalp #(9 9 9)
                 (pmap-into (vector 9 9 9) 'identity (vector))))
     (is (equalp #()
                 (pmap-into (vector) 'identity (vector 1 2 3))))
     (let1 v (make-array 3 :fill-pointer 0)
       (is (equalp #(1 2 3)
                   (pmap-into v 'identity (vector 1 2 3))))
       (is (equalp #(1 2 3) v)))
     (let1 v (make-array 3 :fill-pointer 0)
       (is (equalp #(1 2)
                   (pmap-into v 'identity (vector 1 2))))
       (is (equalp #(1 2) v)))
     (let1 v (make-array 3 :fill-pointer 3)
       (is (equalp #(1 2)
                   (pmap-into v 'identity (vector 1 2))))
       (is (equalp #(1 2) v)))
     (let1 v (make-array 3 :fill-pointer 3)
       (is (equalp #(1)
                   (pmap-into v 'identity :size 1 (vector 1 2))))
       (is (equalp #(1) v)))))

(define-pmap-into-edge-test
    pmap-into-open-edge-test nil)
(define-pmap-into-edge-test
    pmap-into-closed-edge-test (declare (notinline pmap-into)))

(lp-test pmap-compiler-macro-test
  (is (equalp #(1 2 3)
              (pmap 'vector 'identity (vector 1 2 3))))
  (is (equalp #(1 2 3)
              (pmap 'vector 'identity :size 3 (vector 1 2 3))))
  (is (equalp #(1 2)
              (pmap 'vector 'identity :size 2 (vector 1 2 3))))
  (is (equalp #()
              (pmap 'vector 'identity :size 0 (vector 1 2 3))))
  (is (equalp #()
              (pmap 'vector 'identity (vector))))
  (is (equalp #(1 2 3)
              (pmap '(array fixnum (*)) 'identity (vector 1 2 3)))))

(lp-test pmap-compiler-macro-parts-test
  (dotimes (parts 25)
    (let ((src (make-array
                20 :initial-contents (loop :for i :below 20 :collect i)))
          (dst (make-array 20)))
      (is (equalp src (pmap 'vector 'identity src)))
      (is (equalp src (pmap-into dst 'identity src)))
      (is (equalp src dst)))))

(lp-test pmap-notinline-test
  (declare (notinline pmap))
  (is (equalp #(1 2 3)
              (pmap 'vector 'identity (vector 1 2 3))))
  (is (equalp #(1 2 3)
              (pmap 'vector 'identity :size 3 (vector 1 2 3))))
  (is (equalp #(1 2)
              (pmap 'vector 'identity :size 2 (vector 1 2 3))))
  (is (equalp #()
              (pmap 'vector 'identity :size 0 (vector 1 2 3))))
  (is (equalp #()
              (pmap 'vector 'identity (vector))))
  (is (equalp #(1 2 3)
              (pmap '(array fixnum (*)) 'identity (vector 1 2 3)))))
