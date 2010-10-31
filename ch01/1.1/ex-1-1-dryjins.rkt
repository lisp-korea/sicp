#lang racket
;; SICP Exercise 1-1 - Evaluating Combinations Date 2010-10-29 by Dry_Jins
;; Language - DrRacket - Schemme dialet of LISP

;;Exercise 1.1. 
10 ;;10
12 ;;(+ 5 3 4)
8  ;;(- 9 1)
3  ;;(/ 6 2)
6  ;;(+ (* 2 4) (- 4 6))
   ;;(define a 3)
   ;;(define b (+ a 1))
19 ;;(+ a b (* a b))
#f ;;(= a b) That means false.
4  ;;(if (and (> b a) (< b (* a b))) true and true 
   ;;    b
   ;;    a)
   ;;(cond ((= a 4) 6)
   ;;   ((= b 4) (+ 6 7 a))
16 ;;    (else 25))
6  ;;(+ 2 (if (> b a) b a))
16 ;;(* (cond ((> a b) a)
   ;;         ((< a b) b)  4
   ;;         (else -1))
   ;;   (+ a 1))  4

;;Exercise 1.2.
(/ (+ 5 4 
      (- 2
         (- 3 
            (+ 6 (/ 4 5))))) 
   (* 3 (- 6 2) (- 2 7)))

;;Exercise 1.3.
;; square function
(define (square k) (* k k)) 
;; find small one function
(define (small m n) (cond ((> m n) n)
                          (else m))) 
;; sum two bigs squares of three
(define (csq a b c) (- (+ (square a) (square b) (square c)) 
                       (square (small (small a b) c)))) 

(csq 1 2 3) ;; test csq 9 + 4 - 1 = 13

;;Exercise 1.4.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; Test a-plus-abs-b
(a-plus-abs-b 1 -2)
(a-plus-abs-b -2 1)
;; It is allowed.

;;Exercise 1.5.
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;;(test 0 (p)) 
;; In applicative-order, func. p is p(p), so it would run forever because func. p return func. itself but In normal-order, determining x comes first so the result would be zero.

;;Exercise 1.6.
(define (>= s y) (not (< s y)))
(define (abs x) (if (< x 0) (- x) x))
(abs 0)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(new-if (= 2 3) 0 5)

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
(define (sqrt x)
  (sqrt-iter 1.0 x))

