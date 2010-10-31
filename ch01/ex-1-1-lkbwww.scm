;;; exercise1.1
10
;;value:10

(+ 5 3 4)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) < b (* a b))
    b
    a)

(cond ((= a 4) 6 )
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;;;ex 1-2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;;ex1-3
(define (twobigger x y z)
  (define (square x)
    (* x x))
  (cond   ((>= x y) (if (> y z)
                       (+ (square x) (square y))
                       (+ (square x) (square z))))
          ((>= y x) (if (> x z)
                        (+ (square y) (square x))
                        (+ (square y) (square z))))))

;;ex1-4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;;ex1-5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;;;1.1.7 newton method

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;;;ex1-6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;;;ex1-7
(define (new-sqrt x)
  (define (average a b)
    (/ (+ a b) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (abs (- guess (improve guess x))) 0.00001 ))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter (/ x 2.0) x))

;;;ex 1.8
(define (new-sqrt x)
  (define (average a b)
    (/ (+ a b) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (abs (- guess (improve guess x))) 0.00001 ))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter (/ x 2.0) x))

;;;ex1.8
(define (new-curt x)
  (define (square a)
    (* a a))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3 ))
  (define (good-enough? guess x)
    (< (abs (- guess (improve guess))) 0.00001 ))
  (define (curt-iter guess)
    (if (good-enough? guess x)
        guess
        (curt-iter (improve guess))))
  (curt-iter 1.0))
