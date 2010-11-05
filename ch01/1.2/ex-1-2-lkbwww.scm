1.2.1 recursion, iteration process
(define (factorial number)
  (define (fact-iter product counter)
    (if (> counter number)
        product
        (fact-iter (* product counter) (+ counter 1))))
  (fact-iter 1 1))

Exercise 1.9
(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
((defn A [x y]

 (cond (= y 0) 0

       (= x 0) (* 2 y)

       (= y 1) 2

       true (A (- x 1) (A x (- y 1)))))

 

(A 1 10)

=> (A 0 (A 1 9)) => (A 0 (A 0 (A 1 8)))

è  (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))

=> (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2))))))))))

=> 1024

 

(A 2 4) => 65536

 

(A 3 3) => (65536)

(define (f n) (A 0 n)) => (* 2 n)

(defn (g n) (A 1 n)) => n의 제곱

(defn (h n) (A 2 n)) => n의 (A 2 (- n 1))승

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

Iteration
(define (f n)
  (define (f-iter a b c cnt)
    (if (= cnt n)
        a
        (f-iter (+ a (* 2 b) (* 3 c)) a b (+ cnt 1))))
  (if (< n 3)
      n
      (f-iter 4 2 1 3)))


 

1.12

(define (p a b)
  (cond ((= a b) 1)
        ((= b 1) 1)
        ((= a 2) 1)
        ((> b a) 0)
        (else (+ (p (- a 1) (- b 1)) (p (- a 1) b)))))



(defn pascal-triangle [n]

 (defn make-new-line [pre-line new-line cnt]

   (let [max-count (count pre-line)] (if (= (+ cnt 1) max-count)

                                     (conj new-line 1)

                                     (make-new-line pre-line (conj new-line (+ (get pre-line cnt) (get pre-line (+ cnt 1)))) (+ cnt 1)))))

 (defn pascal-iter [pre-line count]

   (if (< count n)

       (do (prn pre-line) (pascal-iter (make-new-line pre-line [1] 0) (+ count 1)))

       (prn pre-line)))
 (cond (= n 1) (prn (vector 1))

       (= n 2) (do (prn (vector 1)) (prn (vector 1 1)))

       true    (do (prn (vector 1)) (pascal-iter (vector 1 1) 2))))

 

1.13~1.16 수학적문제

 

 

 

1.16
;;; recursion
(define (fast-expt b n)
  (cond ((= n 1) 1)
        ((even? n) (squqre (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;;; iteration
(define (fast-expt-iter b n)
  (define (fast-expt-iter-part b m c)
    (cond ((= c 0) m)
          ((even? c) (fast-expt-iter-part (* b b) m (/ c 2)))
          (else (fast-expt-iter-part b (* b m) (- c 1)))))

  (fast-expt-iter-part b 1 n))
;;;1.18
(define (double a)
  (+ a a))
(define (halve a)
  (/ a 2))

(define (fast-* a b)
  (if (even? b)
      (fast-* (double a) (halve b))
      (+ (fast-* a (- b 1)) a)))
(defn fast-* [a b]

 (defn doub [x] (+ x x))

 

 (defn f-iter-even [before cnt]

   (if (<= b cnt) before

      (f-iter-even (+ before (doub a)) (+ cnt 2))))

 

 (defn f-iter-odd []

   (+ a (f-iter-even 0 1)))

 

 (cond (= a 0) 0

       (= b 0) 0

       (even? b) (f-iter-even 0 0)

       true (f-iter-odd)))

 

1.19

덧셈

(defn fast-+ [a b]

 (defn f-iter-even [before cnt]

   (if (<= b cnt) before

      (f-iter-even (+ before 2) (+ cnt 2))))

 

 (defn f-iter-odd []

   (+ a (f-iter-even a 1)))

 

 (cond (= a 0) b

       (= b 0) a

       (= b 1) (+ a 1)

       (even? b) (f-iter-even a 0)

       true (f-iter-odd)))

두배 값
