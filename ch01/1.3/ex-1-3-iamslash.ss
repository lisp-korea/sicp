# -*- coding: utf-8 -*-

;; 
(define (sum-cubes a b)
  (define (cube x)
    (* x x x))
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (identity x) x)
(define (inc x)(+ x 1))

(define (sum-integers a b)
  (sum identity a inc b))

(define (cube x) (* x x x))
(define (square x) (* x x))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;; 1.29 sol
(define (integral-simpson f a b n)
)
