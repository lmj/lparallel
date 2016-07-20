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

(full-test pmap-into-test
  (let ((a (list nil nil nil)))
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equal '(15 17 19) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equal '(15 17 19) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equal '(15 17 19) a)))
  (let ((a (list nil)))
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equal '(15) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equal '(15) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equal '(15) a)))
  (let ((a (vector nil nil nil)))
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equalp #(15 17 19) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equalp #(15 17 19) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equalp #(15 17 19) a)))
  (let ((a (vector nil)))
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equalp #(15) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equalp #(15) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equalp #(15) a))))

(full-test degenerate-pmaps-test
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

(full-test pmap-nil-test
  (loop for n in '(0 1 2 3 4 5 6 7 8 9 10 100 1000)
        do (let ((a (loop for x from 0 repeat n collect x))
                 (b (loop for x from 0 repeat n collect (* 2 x)))
                 (q (make-queue)))
             (pmap nil (lambda (x y) (push-queue (+ x y) q)) a b)
             (is (equal (sort (extract-queue q) #'<)
                        (loop for x from 0 repeat n collect (* 3 x)))))))

(full-test pmapcar-test
  (is (equal '(15 17 19)
             (pmapcar '+ '(5 6 7) '(10 11 12))))
  (is (equal '(15 17 19)
             (pmapcar '+ :parts 3 '(5 6 7) '(10 11 12)))))

(full-test pmapcar-handles-sequences-test
  (is (equal (mapcar  '+ '(1 2 3) '(4 5 6))
             (pmapcar '+ '(1 2 3) #(4 5 6))))
  (is (equal (mapcar  '+ '(1 2 3) '(4 5 6))
             (pmapcar '+ :parts 3 '(1 2 3) #(4 5 6)))))

(full-test grind-pmap-test
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
      (let ((result (make-list 500)))
        (is (equal expected (apply #'pmap-into result args)))
        (is (equal expected result)))
      (let ((result (make-list 500)))
        (is (equal expected (apply #'pmap-into result #'f :parts 500 lists)))
        (is (equal expected result)))
      (dolist (parts '(nil 1000))
        (setf *memo* (make-queue))
        (apply #'pmapc
               (lambda (i x y z)
                 (push-queue (cons i (f x y z)) *memo*))
               :parts parts
               (loop for i from 0 below (apply #'min (mapcar #'length lists))
                     collect i)
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

(full-test pmaplist-test
  (is (equalp (maplist  #'vector '(a b c) '(1 2 3))
              (pmaplist #'vector '(a b c) '(1 2 3)))))

(full-test grind-pmaplist-test
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
           (loop for i from 0 below (apply #'min (mapcar #'length lists))
                 collect i)
           lists)
    (is (equalp expected
                (map 'list
                     #'cadr
                     (sort (extract-queue *memo*) '< :key #'caar))))))

(full-test preduce-partial-test
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

(full-test grind-preduce-test
  (is (= 3
         (reduce  (constantly 3) nil)
         (preduce (constantly 3) nil)))
  (is (= 3
         (reduce  (constantly 5) nil :initial-value 3)
         (preduce (constantly 5) nil :initial-value 3)))
  (flet ((non-associative/non-commutative (x y)
           (+ (* 2 x) y))
         (associative/non-commutative (a b)
           (vector (+ (* (aref a 0) (aref b 0)) (* (aref a 1) (aref b 2)))
                   (+ (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 3)))
                   (+ (* (aref a 2) (aref b 0)) (* (aref a 3) (aref b 2)))
                   (+ (* (aref a 2) (aref b 1)) (* (aref a 3) (aref b 3)))))
         (verify (test &rest args)
           (loop for parts from 1 to 10
                 do (is (funcall test
                                 (apply #'reduce args)
                                 (apply #'preduce args)))
                    (is (funcall test
                                 (apply #'reduce args)
                                 (apply #'preduce
                                        (append args (list :parts parts)))))
                    (is (funcall test
                                 (apply #'reduce args)
                                 (apply #'preduce
                                        (append args (list :from-end t)))))
                    (is (funcall test
                                 (apply #'reduce args)
                                 (apply #'preduce
                                        (append args (list :recurse t)))))
                    (is (funcall test
                                 (apply #'reduce args)
                                 (apply #'preduce
                                        (append args (list :recurse t
                                                           :parts parts)))))
                    (is (funcall test
                                 (apply #'reduce args)
                                 (apply #'preduce
                                        (append args (list :recurse t
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

      (let ((serial (reduce #'non-associative/non-commutative c)))
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

(full-test grind-pevery-test
  (flet ((verify (&rest args)
           (loop for (regular parallel) in '((some     psome)
                                             (every    pevery)
                                             (notany   pnotany)
                                             (notevery pnotevery))
                 do (is (eql (apply regular args)
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

(full-test parts-arg-test
  (flet ((sq (x) (* x x)))
    (loop for parts from 1 to 8
          do (loop for n from 1 to 6
                   do (let ((a (collect-n n (random n))))
                        (is (equalp ( map-into (make-array n) #'sq a)
                                    (pmap-into (make-array n) #'sq
                                               :parts parts a)))
                        (is (equal  ( map-into (make-list n) #'sq a)
                                    (pmap-into (make-list n) #'sq
                                               :parts parts a)))
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

(defmacro define-plet-test (test-name fn-name defun store-value-p)
  ;; use assert since this may execute in another thread
  `(progn
     (,defun ,fn-name ()
       (plet ((a 3)
              (b 4))
         (assert (= 7 (+ a b))))
       (let ((handledp nil))
         (block done
           (handler-bind ((client-error (lambda (e)
                                          (declare (ignore e))
                                          (setf handledp t)
                                          (return-from done))))
             (task-handler-bind ((error (lambda (e)
                                          (invoke-restart 'transfer-error e))))
               (plet ((a (error 'client-error)))
                 a))))
         (assert handledp))
       ,(when store-value-p
          `(task-handler-bind ((error (lambda (e)
                                        (invoke-restart 'transfer-error e))))
             (handler-bind ((error (lambda (e)
                                     (declare (ignore e))
                                     (invoke-restart 'store-value 4))))
               (setf *memo* (lambda () (error "foo")))
               (plet ((a 3)
                      (b (funcall *memo*)))
                 (assert (= 7 (+ a b))))))))
     (full-test ,test-name
       (,fn-name)
       (is (= 1 1)))))

(define-plet-test plet-test plet-test-fn defun t)

(base-test plet-if-test
  (setf *memo* 0)
  (plet-if (plusp *memo*)
      ((a 3))
    (is (= 3 a)))
  (signals no-kernel-error
    (plet-if (zerop *memo*)
        ((a 3))
      (is (= 3 a)))))

(full-test plet-type-declaration-test
  (plet ((x 3))
    (declare (type t x))
    (is (= 3 x)))
  (plet ((x 3))
    (declare (fixnum x))
    (is (= 3 x)))
  (plet (((x) 3))
    (declare (fixnum x))
    (is (= 3 x)))
  (plet (((x y) (values 3 4)))
    (declare (type fixnum x y))
    (is (= 3 x))
    (is (= 4 y)))
  (plet ((x 3))
    (declare (fixnum x))
    (declare (integer x))
    (is (= 3 x)))
  (plet ((x 3))
    (declare (type fixnum x))
    (declare (type t x))
    (declare (integer x))
    (is (= 3 x)))
  (plet ((x 3) y)
    (declare (fixnum x))
    (is (equal '(3 nil) (list x y))))
  (plet ((x 3) (y))
    (declare (fixnum x))
    (is (equal '(3 nil) (list x y))))
  (plet ((x 3) y)
    (declare (fixnum x)
             (type null y))
    (is (equal '(3 nil) (list x y))))
  (plet ((x 3) (y))
    (declare (fixnum x)
             (type null y))
    (is (equal '(3 nil) (list x y)))))

(full-test pand-por-test
  (is (null (pand 3 4 5 6 nil)))
  (is (null (pand 3 4 nil 5 6)))
  (is (null (pand nil 3 4 5 6)))
  (is (member (pand 3 4 5 6) '(3 4 5 6)))

  (is (member (por 3 4 5 6 nil) '(3 4 5 6)))
  (is (member (por 3 4 nil 5 6) '(3 4 5 6)))
  (is (member (por nil 3 4 5 6) '(3 4 5 6)))

  (when (> (kernel-worker-count) 2)
    (sleep 0.4)
    (is (eql 4 (por  (progn (sleep 0.2) 3) 4)))
    (sleep 0.4)
    (is (eql 3 (pand (progn (sleep 0.2) 3) 4)))

    (sleep 0.4)
    (is (eql 4 (por  nil (progn (sleep 0.2) 3) 4)))
    (sleep 0.4)
    (is (eql 4 (por  (progn (sleep 0.2) 3) nil 4)))

    (sleep 0.4)
    (is (null (pand nil (progn (sleep 0.2) 3) 4)))
    (sleep 0.4)
    (is (null (pand (progn (sleep 0.2) 3) nil 4)))))

(full-test psort-test
  ;; abcl workarounds for worse-case sort bug
  (dolist (granularity '(nil 1 5 100))
    (dolist (size #-lparallel.with-green-threads '(1 5 10 100 10000)
                  #+lparallel.with-green-threads '(1 5 10))
      (let ((source (make-random-vector size)))
        (let ((a (copy-seq source))
              (b (copy-seq source)))
          (is (equalp ( sort a #'<)
                      (psort b #'< :granularity granularity)))
          #-abcl
          (is (equalp ( sort a #'<)
                      (psort b #'< :granularity granularity)))
          #-abcl
          (is (equalp ( sort a #'>)
                      (psort b #'> :granularity granularity)))
          #-abcl
          (is (equalp ( sort a #'>)
                      (psort b #'> :granularity granularity)))))
      (let ((source (make-random-vector size)))
        (let ((a (copy-seq source))
              (b (copy-seq source)))
          (is (equalp ( sort a '< :key '-)
                      (psort b '< :key '- :granularity granularity)))
          #-abcl
          (is (equalp ( sort a '< :key #'-)
                      (psort b '< :key #'- :granularity granularity)))
          #-abcl
          (is (equalp ( sort a #'> :key '-)
                      (psort b #'> :key '- :granularity granularity)))
          #-abcl
          (is (equalp ( sort a #'> :key #'-)
                      (psort b #'> :key #'- :granularity granularity))))))
    (let ((source (vector 5 1 9 3 6 0 1 9)))
      (let ((a (copy-seq source))
            (b (copy-seq source)))
        (is (equalp ( sort a #'<)
                    (psort b #'< :granularity granularity)))
        #-abcl
        (is (equalp ( sort a #'<)
                    (psort b #'< :granularity granularity)))
        #-abcl
        (is (equalp ( sort a #'>)
                    (psort b #'> :granularity granularity)))
        #-abcl
        (is (equalp ( sort a #'>)
                    (psort b #'> :granularity granularity)))))
    (let ((source (vector 5 1 9 3 6 0 1 9)))
      (let ((a (copy-seq source))
            (b (copy-seq source)))
        (is (equalp ( sort a #'< :key (lambda (x) (* -1 x)))
                    (psort b #'< :key (lambda (x) (* -1 x))
                            :granularity granularity)))
        #-abcl
        (is (equalp ( sort a #'< :key (lambda (x) (* -1 x)))
                    (psort b #'< :key (lambda (x) (* -1 x))
                            :granularity granularity)))
        #-abcl
        (is (equalp ( sort a #'> :key (lambda (x) (* -1 x)))
                    (psort b #'> :key (lambda (x) (* -1 x))
                            :granularity granularity)))
        #-abcl
        (is (equalp ( sort a #'> :key (lambda (x) (* -1 x)))
                    (psort b #'> :key (lambda (x) (* -1 x))
                            :granularity granularity)))))
    (let ((source (make-array 50 :initial-element 5)))
      (let ((a (copy-seq source))
            (b (copy-seq source)))
        (is (equalp ( sort a #'<)
                    (psort b #'< :granularity granularity)))
        #-abcl
        (is (equalp ( sort a #'<)
                    (psort b #'< :granularity granularity)))
        #-abcl
        (is (equalp ( sort a #'>)
                    (psort b #'> :granularity granularity)))
        #-abcl
        (is (equalp ( sort a #'>)
                    (psort b #'> :granularity granularity)))))))

(full-test premove-if-test
  (loop for size below 100
        for where = (random 1.0)
        for source = (collect-n size (random 1.0))
        do (is (equal (remove-if  (partial-apply #'< where) source)
                      (premove-if (partial-apply #'< where) source)))))

(full-test second-premove-if-test
  (loop for (std par) in '((remove-if-not premove-if-not)
                           (remove-if     premove-if))
        do (loop for size below 100
                 for where = (random 1.0)
                 for a = (make-random-list size)
                 for b = (make-random-vector size)
                 do (is (equal (funcall std (partial-apply #'< where) a)
                               (funcall par (partial-apply #'< where) a)))
                    (is (equalp (funcall std (partial-apply #'< where) b)
                                (funcall par (partial-apply #'< where) b)))
                    (when (>= size 77)
                      (is (equal (funcall std (partial-apply #'< where) a
                                          :start 20)
                                 (funcall par (partial-apply #'< where) a
                                          :start 20)))
                      (is (equal (funcall std (partial-apply #'< where) a
                                          :start 20 :end 77)
                                 (funcall par (partial-apply #'< where) a
                                          :start 20 :end 77)))))))

(full-test premove-test
  (loop for size below 100
        for where = (random 1.0)
        for a = (make-random-list size)
        for b = (make-random-vector size)
        do (is (equal (remove  where a :test #'<)
                      (premove where a :test #'<)))
           (is (equal (remove  where a :test-not #'>=)
                      (premove where a :test-not #'>=)))
           (is (equalp (remove  where b :test #'<)
                       (premove where b :test #'<)))
           (is (equalp (remove  where b :test-not (complement #'<))
                       (premove where b :test-not (complement #'<)))))
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
  (let ((x (cons nil nil)))
    (is (equal (remove  x (list 3 4 x 4 9 x 2))
               (premove x (list 3 4 x 4 9 x 2)))))
  (let ((x (cons nil nil)))
    (is (equalp (remove  x (make-array
                            7
                            :adjustable t
                            :initial-contents (list 3 4 x 4 9 x 2)))
                (premove x (make-array
                            7
                            :adjustable t
                            :initial-contents (list 3 4 x 4 9 x 2)))))))

(define-condition foo-warning (warning) ())

(base-test worker-context-test
  (flet ((my-worker-context (fn)
           (handler-bind ((warning (lambda (e)
                                     (declare (ignore e))
                                     (invoke-restart 'double-me 3))))
             (funcall fn))))
    (dolist (n '(1 2 3 4 5 6 10))
      (let ((result (with-temp-kernel (n :context #'my-worker-context)
                      (pmapcar (lambda (x)
                                 (declare (ignore x))
                                 (restart-case (warn 'foo-warning)
                                   (double-me (z)
                                     ;; clisp warns unless interactive is given
                                     :interactive (lambda ())
                                     (* 2 z))))
                               '(3 3)))))
        (is (equal '(6 6) result))))))

(full-test cognate-handler-test
  (task-handler-bind ((foo-error (lambda (e)
                                   (declare (ignore e))
                                   (invoke-restart 'something-else 3))))
    (is (equal '(3 3)
               (pmapcar (lambda (x)
                          (declare (ignore x))
                          (restart-case (error 'foo-error)
                            (something-else (z)
                              ;; clisp warns unless interactive is given
                              :interactive (lambda ())
                              z)))
                        '(0 1))))))

(full-test pmap-handler-test
  (task-handler-bind ((foo-error
                       (lambda (e) (invoke-restart 'transfer-error e))))
    (signals foo-error
      (pmapcar (lambda (x)
                 (declare (ignore x))
                 (error 'foo-error))
               '(3 4 5 6)))))

(full-test pmap-restart-test
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

(full-test pmap-into-bounds-test
  (dotimes (i 3)
    (dotimes (j (1+ i))
      (let ((contents (collect-n i (random 1000))))
        (destructuring-bind (a b)
            (collect-n 2
              (make-array i :fill-pointer j :initial-contents contents))
          (dotimes (k 6)
            (let ((source (collect-n k (random 1000))))
              (let ((c (pmap-into b #'identity source))
                    (d (map-into  a #'identity source)))
                (is (equalp a b))
                (is (equalp c d))))))))))

(full-test pmap-with-size-constraint-test
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

(full-test pmap-into-list-test
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

(full-test pmap-into-degenerate-input-test
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

(full-test pmaplist-into-degenerate-input-test
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

(full-test pfuncall-test
  (is (= 7 (pfuncall '+ 3 4)))
  (let ((memo (make-queue)))
    (is (= 7 (pfuncall
              #'+
              (progn (sleep 0.2) (push-queue 3 memo) 3)
              (progn (sleep 0.2) (push-queue 4 memo) 4))))
    (sleep 0.3)
    (is (= 2 (queue-count memo)))))

(full-test pcount-if-test
  (is (zerop (pcount-if 'non-function '())))
  (is (zerop (pcount-if 'non-function #())))
  (signals error
    (pcount-if 'non-function '() :start 2))
  (loop for size from 1 below 100
        for where = (random 1.0)
        for source = (collect-n size (random 1.0))
        do (is (equal (count-if  (partial-apply #'< where) source)
                      (pcount-if (partial-apply #'< where) source)))))

(full-test second-pcount-if-test
  (loop for (std par) in '((count-if-not pcount-if-not)
                           (count-if     pcount-if))
        do (loop for size from 1 below 100
                 for where = (random 1.0)
                 for a = (make-random-list size)
                 for b = (make-random-vector size)
                 do (is (equal  (funcall std (partial-apply #'< where) a)
                                (funcall par (partial-apply #'< where) a)))
                    (is (equalp (funcall std (partial-apply #'< where) b)
                                (funcall par (partial-apply #'< where) b)))
                    (when (>= size 77)
                      (is (equal (funcall std (partial-apply #'< where) a
                                          :start 20)
                                 (funcall par (partial-apply #'< where) a
                                          :start 20)))
                      (is (equal (funcall std (partial-apply #'< where) a
                                          :start 20 :end 77)
                                 (funcall par (partial-apply #'< where) a
                                          :start 20 :end 77)))))))

(full-test pcount-test
  (loop for size from 1 below 100
        for where = (random 1.0)
        for a = (make-random-list size)
        for b = (make-random-vector size)
        do (is (equal  (count  where a :test #'<)
                       (pcount where a :test #'<)))
           (is (equal  (count  where a :test-not (complement #'<))
                       (pcount where a :test-not (complement #'<))))
           (is (equalp (count  where b :test #'<)
                       (pcount where b :test #'<)))
           (is (equalp (count  where b :test-not (complement #'<))
                       (pcount where b :test-not (complement #'<)))))
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
  (let ((x (cons nil nil)))
    (is (equal (count  x (list 3 4 x 4 9 x 2))
               (pcount x (list 3 4 x 4 9 x 2)))))
  (let ((x (cons nil nil)))
    (is (equalp (count  x (make-array
                           7
                           :adjustable t
                           :initial-contents (list 3 4 x 4 9 x 2)))
                (pcount x (make-array
                           7
                           :adjustable t
                           :initial-contents (list 3 4 x 4 9 x 2)))))))

(full-test pfind-if-test
  (signals error (pfind-if 'non-function '()))
  (signals error (pfind-if 'non-function #()))
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
  (loop for size from 1 below 100
        for source = (collect-n size (random 1.0))
        do (setf (elt source (random size)) 999)
           (is (eql (find-if  (partial-apply #'eql 999) source)
                    (pfind-if (partial-apply #'eql 999) source)))))

(full-test second-pfind-if-test
  (loop for (std par) in '((find-if pfind-if))
        do (loop for size from 1 below 100
                 for a = (make-random-list size)
                 for b = (make-random-vector size)
                 for target = (let ((index (random size)))
                                (setf (elt a index) 99.0
                                      (elt b index) 99.0))
                 do (is (equal  (funcall std (partial-apply #'eql target) a)
                                (funcall par (partial-apply #'eql target) a)))
                    (is (equalp (funcall std (partial-apply #'eql target) b)
                                (funcall par (partial-apply #'eql target) b)))
                    (when (>= size 77)
                      (is (equal (funcall std (partial-apply #'eql target) a
                                          :start 20)
                                 (funcall par (partial-apply #'eql target) a
                                          :start 20)))
                      (is (equal (funcall std (partial-apply #'eql target) a
                                          :start 20 :end 77)
                                 (funcall par (partial-apply #'eql target) a
                                          :start 20 :end 77)))))))

(full-test pfind-test
  (signals error
    (pfind 3 '(3 3 3) :test #'eql :test-not #'eql))
  (loop for size from 1 below 100
        for a = (make-random-list size)
        for b = (make-random-vector size)
        for target = (let ((index (random size)))
                       (setf (elt a index) 99.0
                             (elt b index) 99.0))
        do (is (equal  (find  target a)
                       (pfind target a)))
           (is (equalp (find  target b)
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
  (let ((x (cons nil nil)))
    (is (equal (find  x (list 3 4 x 4 9 x 2))
               (pfind x (list 3 4 x 4 9 x 2)))))
  (let ((x (cons nil nil)))
    (is (equalp (find  x (make-array
                          7
                          :adjustable t
                          :initial-contents (list 3 4 x 4 9 x 2)))
                (pfind x (make-array
                          7
                          :adjustable t
                          :initial-contents (list 3 4 x 4 9 x 2)))))))

(defmacro define-pmap-into-edge-test (name decl)
  `(full-test ,name
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
     (let ((v (make-array 3 :fill-pointer 0)))
       (is (equalp #(1 2 3)
                   (pmap-into v 'identity (vector 1 2 3))))
       (is (equalp #(1 2 3) v)))
     (let ((v (make-array 3 :fill-pointer 0)))
       (is (equalp #(1 2)
                   (pmap-into v 'identity (vector 1 2))))
       (is (equalp #(1 2) v)))
     (let ((v (make-array 3 :fill-pointer 0)))
       (is (equalp #(9 9 9)
                   (pmap-into v (constantly 9))))
       (is (equalp #(9 9 9) v)))
     (let ((v (make-array 3 :fill-pointer 1)))
       (is (equalp #(9 9 9)
                   (pmap-into v (constantly 9))))
       (is (equalp #(9 9 9) v)))
     (let ((v (make-array 3 :fill-pointer 2)))
       (is (equalp #(9 9 9)
                   (pmap-into v (constantly 9))))
       (is (equalp #(9 9 9) v)))
     (let ((v (make-array 3 :fill-pointer 3)))
       (is (equalp #(9 9 9)
                   (pmap-into v (constantly 9))))
       (is (equalp #(9 9 9) v)))
     (let ((v (make-array 3 :fill-pointer 3)))
       (is (equalp #(1 2)
                   (pmap-into v 'identity (vector 1 2))))
       (is (equalp #(1 2) v)))
     (let ((v (make-array 3 :fill-pointer 3)))
       (is (equalp #(1)
                   (pmap-into v 'identity :size 1 (vector 1 2))))
       (is (equalp #(1) v)))))

(define-pmap-into-edge-test
    pmap-into-open-edge-test nil)
(define-pmap-into-edge-test
    pmap-into-closed-edge-test (declare (notinline pmap-into)))

(full-test pmap-compiler-macro-test
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

(full-test pmap-compiler-macro-parts-test
  (dotimes (parts 25)
    (let ((src (make-array
                20 :initial-contents (loop for i below 20 collect i)))
          (dst (make-array 20)))
      (is (equalp src (pmap 'vector 'identity src)))
      (is (equalp src (pmap-into dst 'identity src)))
      (is (equalp src dst)))))

(full-test pmap-notinline-test
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

(full-test cognate-steal-test
  (let ((channel (make-channel)))
    (submit-task channel
                 (lambda ()
                   (pmap 'vector 'identity '(1 2 3 4 5))))
    (is (equalp #(1 2 3 4 5) (receive-result channel))))
  (let ((channel (make-channel)))
    (submit-task channel
                 (lambda ()
                   (pmap-reduce 'identity '+ '(1 2 3 4 5))))
    (is (eql 15 (receive-result channel))))
  (let ((channel (make-channel)))
    (submit-task channel
                 (lambda ()
                   (por nil nil 5)))
    (is (eql 5 (receive-result channel))))
  (is (not (null (pand 9 (por nil 3)))))
  (is (eql 3 (por nil nil (por nil 3) (por nil nil 3))))
  (let ((channel (make-channel)))
    (submit-task channel
                 (lambda ()
                   (let* ((a (make-random-vector 1000))
                          (b (copy-seq a)))
                     (list (sort a #'<)
                           (psort b #'<)))))
    (is (apply #'equalp (receive-result channel)))))

(base-test cognate-steal-priority-test
  (with-temp-kernel (2)
    (let ((channel (make-channel))
          (flag nil))
      (with-thread ()
        (sleep 1.5)
        (setf flag t))
      (submit-task channel
                   (lambda ()
                     (pmap nil #'sleep '(1 1))))
      (receive-result channel)
      (is (eq nil flag))
      (sleep 1)
      (is (eq t flag))))
  (with-temp-kernel (2)
    (let ((channel (make-channel))
          (flag nil))
      (with-thread ()
        (sleep 1.5)
        (setf flag t))
      (submit-task channel
                   (lambda ()
                     (let ((*task-priority* :low))
                       (pmap nil #'sleep '(1 1)))))
      (receive-result channel)
      (is (eq nil flag))
      (sleep 1)
      (is (eq t flag)))))

(full-test pdotimes-test
  (dotimes (n 100)
    (flet ((f (x) (* x x)))
      (let ((a (make-array n))
            (b (make-array n)))
        (dotimes (i n)
          (setf (aref a i) (f i)))
        (pdotimes (i n)
          (setf (aref b i) (f i)))
        (is (equalp a b)))))
  (dotimes (parts 100)
    (flet ((f (x) (* x x)))
      (let ((a (make-array 100))
            (b (make-array 100)))
        (dotimes (i 100)
          (setf (aref a i) (f i)))
        (pdotimes (i 100 :discard (1+ parts))
          (setf (aref b i) (f i)))
        (is (equalp a b)))))
  (dotimes (n 2)
    (is (eq (dotimes (i n :foo)
              (declare (ignorable i)))
            (pdotimes (i n :foo)
              (declare (ignorable i))))))
  (dotimes (n 4)
    (is (eq (dotimes (i n (* i i))
              (declare (ignorable i)))
            (pdotimes (i n (* i i))
              (declare (ignorable i)))))))

(full-test pdotimes-second-test
  (signals error
    (pdotimes (i 4.0)
      (declare (ignore i))))
  (pdotimes (i 0)
    (declare (ignore i))
    (error "oops"))
  (pdotimes (i -1)
    (declare (ignore i))
    (error "oops"))
  (setf *memo* 1)
  (is (= (dotimes (i *memo* i)
           (declare (ignorable i)))
         (pdotimes (i *memo* i)
           (declare (ignorable i)))))
  (setf *memo* 0)
  (is (= (dotimes (i *memo* i)
           (declare (ignorable i)))
         (pdotimes (i *memo* i)
           (declare (ignorable i)))))
  (setf *memo* -1)
  (is (= (dotimes (i *memo* i)
           (declare (ignorable i)))
         (pdotimes (i *memo* i)
           (declare (ignorable i)))))
  (setf *memo* t)
  (let ((q (make-queue)))
    (dotimes (i 4)
      (when *memo* (go :end))
      (error "skip me")
      :end
      (push-queue i q))
    (is (equal '(0 1 2 3) (sort (extract-queue q) '<))))
  (let ((q (make-queue)))
    (pdotimes (i 4)
      (when *memo* (go :end))
      (error "skip me")
      :end
      (push-queue i q))
    (is (equal '(0 1 2 3) (sort (extract-queue q) '<)))))

(full-test function-designators-test
  (is (eql (pcount 3 '(1 2 3) :test 'eql :key 'identity)
           ( count 3 '(1 2 3) :test 'eql :key 'identity)))
  (is (eql (pcount 3 '(1 2 3) :test #'eql :key #'identity)
           ( count 3 '(1 2 3) :test #'eql :key #'identity)))
  (is (eql (pcount 3 '(1 2 3) :test-not 'eql :key 'identity)
           ( count 3 '(1 2 3) :test-not 'eql :key 'identity)))
  (is (eql (pcount 3 '(1 2 3) :test-not #'eql :key #'identity)
           ( count 3 '(1 2 3) :test-not #'eql :key #'identity)))

  (is (eql (pcount-if  'oddp '(1 2 3) :key 'identity)
           ( count-if  'oddp '(1 2 3) :key 'identity)))
  (is (eql (pcount-if #'oddp '(1 2 3) :key #'identity)
           ( count-if #'oddp '(1 2 3) :key #'identity)))

  (is (eql (pcount-if-not  'oddp '(1 2 3) :key 'identity)
           ( count-if-not  'oddp '(1 2 3) :key 'identity)))
  (is (eql (pcount-if-not #'oddp '(1 2 3) :key #'identity)
           ( count-if-not #'oddp '(1 2 3) :key #'identity)))

  (is (eql (pevery  'oddp '(1 2 3))
           ( every  'oddp '(1 2 3))))
  (is (eql (pevery #'oddp '(1 2 3))
           ( every #'oddp '(1 2 3))))
  (is (eql (psome  'oddp '(1 2 3))
           ( some  'oddp '(1 2 3))))
  (is (eql (psome #'oddp '(1 2 3))
           ( some #'oddp '(1 2 3))))
  (is (eql (pnotany  'oddp '(1 2 3))
           ( notany  'oddp '(1 2 3))))
  (is (eql (pnotany #'oddp '(1 2 3))
           ( notany #'oddp '(1 2 3))))
  (is (eql (pnotevery  'oddp '(1 2 3))
           ( notevery  'oddp '(1 2 3))))
  (is (eql (pnotevery #'oddp '(1 2 3))
           ( notevery #'oddp '(1 2 3))))

  (is (eql (pfind 2 '(1 2) :test 'eql :key 'identity)
           ( find 2 '(1 2) :test 'eql :key 'identity)))
  (is (eql (pfind 2 '(1 2) :test #'eql :key #'identity)
           ( find 2 '(1 2) :test #'eql :key #'identity)))
  (is (eql (pfind 2 '(1 2) :test-not 'eql :key 'identity)
           ( find 2 '(1 2) :test-not 'eql :key 'identity)))
  (is (eql (pfind 2 '(1 2) :test-not #'eql :key #'identity)
           ( find 2 '(1 2) :test-not #'eql :key #'identity)))

  (is (eql (pfind-if  'oddp '(1 2) :key 'identity)
           ( find-if  'oddp '(1 2) :key 'identity)))
  (is (eql (pfind-if #'oddp '(1 2) :key #'identity)
           ( find-if #'oddp '(1 2) :key #'identity)))

  (is (eql (pfind-if-not  'oddp '(1 2) :key 'identity)
           ( find-if-not  'oddp '(1 2) :key 'identity)))
  (is (eql (pfind-if-not #'oddp '(1 2) :key #'identity)
           ( find-if-not #'oddp '(1 2) :key #'identity)))

  (is (eql (pfuncall  '+ 1 2)
           ( funcall  '+ 1 2)))
  (is (eql (pfuncall #'+ 1 2)
           ( funcall #'+ 1 2)))

  (is (equal  (pmap 'list 'identity '(1 2))
              ( map 'list 'identity '(1 2))))
  (is (equal  (pmap 'list #'identity '(1 2))
              ( map 'list #'identity '(1 2))))
  (is (equalp (pmap 'vector 'identity '(1 2))
              ( map 'vector 'identity '(1 2))))
  (is (equalp (pmap 'vector #'identity '(1 2))
              ( map 'vector #'identity '(1 2))))
  (is (equalp (apply #'pmap 'vector 'identity '((1 2)))
              (apply #'map  'vector 'identity '((1 2)))))
  (is (equalp (apply #'pmap 'vector #'identity '((1 2)))
              (apply #'map  'vector #'identity '((1 2)))))

  (is (equal (pmapc 'identity '(1 2))
             ( mapc 'identity '(1 2))))
  (is (equal (pmapc #'identity '(1 2))
             ( mapc #'identity '(1 2))))

  (is (equal (pmapcan 'list '(1 2))
             ( mapcan 'list '(1 2))))
  (is (equal (pmapcan #'list '(1 2))
             ( mapcan #'list '(1 2))))

  (is (equal (pmapcar 'identity '(1 2))
             ( mapcar 'identity '(1 2))))
  (is (equal (pmapcar #'identity '(1 2))
             ( mapcar #'identity '(1 2))))

  (is (equal (pmapcon 'list '(1 2))
             ( mapcon 'list '(1 2))))
  (is (equal (pmapcon #'list '(1 2))
             ( mapcon #'list '(1 2))))

  (is (equal (pmap-into (list 0 0)  'identity '(1 2))
             ( map-into (list 0 0)  'identity '(1 2))))
  (is (equal (pmap-into (list 0 0) #'identity '(1 2))
             ( map-into (list 0 0) #'identity '(1 2))))

  (is (equal (pmapl 'identity '(1 2))
             ( mapl 'identity '(1 2))))
  (is (equal (pmapl #'identity '(1 2))
             ( mapl #'identity '(1 2))))

  (is (equal (pmaplist 'identity '(1 2))
             ( maplist 'identity '(1 2))))
  (is (equal (pmaplist #'identity '(1 2))
             ( maplist #'identity '(1 2))))

  (is (equal (pmaplist-into (list 0 0) 'identity '(1 2))
             ( maplist 'identity '(1 2))))
  (is (equal (pmaplist-into (list 0 0) #'identity '(1 2))
             ( maplist #'identity '(1 2))))

  (is (eql (preduce '+  '(1 2 3) :key 'identity)
           ( reduce '+  '(1 2 3) :key 'identity)))
  (is (eql (preduce #'+ '(1 2 3) :key #'identity)
           ( reduce #'+ '(1 2 3) :key #'identity)))

  (is (equalp (preduce-partial '+ '(1 2 3) :key 'identity :parts 1)
              #(6)))
  (is (equalp (preduce-partial #'+ '(1 2 3) :key #'identity :parts 1)
              #(6)))

  (is (equal (premove 2 '(1 2) :test 'eql :key 'identity)
             ( remove 2 '(1 2) :test 'eql :key 'identity)))
  (is (equal (premove 2 '(1 2) :test #'eql :key #'identity)
             ( remove 2 '(1 2) :test #'eql :key #'identity)))

  (is (equal (premove-if 'oddp '(1 2) :key 'identity)
             ( remove-if 'oddp '(1 2) :key 'identity)))
  (is (equal (premove-if #'oddp '(1 2) :key #'identity)
             ( remove-if #'oddp '(1 2) :key #'identity)))

  (is (equalp (psort (vector 1 3 2) '<)
              ( sort (vector 1 3 2) '<)))
  (is (equalp (psort (vector 1 3 2) #'<)
              ( sort (vector 1 3 2) #'<))))

(base-test slet-test
  (let ((z 0))
    (slet ((a (incf z)) (b (incf z)))
      (is (= 1 a))
      (is (= 2 b))))
  (let ((z 0))
    (slet (((a) (incf z)) (b (incf z)))
      (is (= 1 a))
      (is (= 2 b))))
  (let ((z 0))
    (slet ((a (incf z))
           ((b c) (values (incf z) (incf z)))
           (d (incf z))
           ((e f g) (values (incf z) (incf z) (incf z))))
      (is (equal '(1 2 3 4 5 6 7) (list a b c d e f g)))))
  (slet (a (b) ((c)) d (e))
    (declare (type null d c))
    (is (equal '(nil nil nil nil nil) (list a b c d e)))))

(base-test slet-unbound-test
  (signals error
    (funcall (compile/muffled nil '(lambda ()
                                     (slet ((a 3)
                                            (b (1+ a)))
                                       (list a b)))))))

(full-test plet-multiple-value-test
  (plet ((a 1) (b 2))
    (is (= 1 a))
    (is (= 2 b)))
  (plet (((a) 1) (b 2))
    (is (= 1 a))
    (is (= 2 b)))
  (plet ((a 1)
         ((b c) (values 2 3))
         (d 4)
         ((e f g) (values 5 6 7)))
    (is (equal '(1 2 3 4 5 6 7) (list a b c d e f g))))
  (plet ((a) b)
    (is (null a))
    (is (null b)))
  (plet (((a b)))
    (is (null a))
    (is (null b))))

(base-test plet-unbound-test
  (signals error
    (task-handler-bind ((error #'invoke-transfer-error))
      (funcall (compile/muffled nil '(lambda ()
                                       (plet ((a 3)
                                              (b (1+ a)))
                                         (list a b))))))))
