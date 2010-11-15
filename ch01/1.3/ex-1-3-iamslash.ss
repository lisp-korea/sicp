;; # -*- coding: utf-8 -*-

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
;; 왜 안되지???

(define (integral-simpson f a b n)
  (define (sum-simpson term a next b k n)
    (cond ((> a b) 0)
          ((or (= k 0) (= k n))
           (+ (term a) (sum-simpson term (next a) next b (+ k 1) n)))
          ((= (remainder k 2) 1)
           ((+ (* (term a) 4)
               (sum-simpson term (next a) next b (+ k 1) n))))
          (else
           ((+ (* (term a) 2)
               (sum-simpson term (next a) next b (+ k 1) n))))))
  (define (get-h a b n)
    (/ (- b a) n))
  (define (add-dx x)
    (+ x (/ (- b a) n)))
  (* (sum-simpson f a add-dx b 0 n) 
     (/ (get-h a b n) 3)))

;; 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; 1.31
        