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

;; linear recursion
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; iterative recursion
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))


;; 1.32.a

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (define (add x y)
    (+ x y))
  (accumulate add 0 term a next b))
(define (product term a next b)
  (define (mult x y) (* x y))
  (accumulate mult 1 term a next b))  

;; 1.32.b
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))    
(define (sum term a next b)
  (define (add x y)
    (+ x y))
  (accumulate add 0 term a next b))
(define (product term a next b)
  (define (mult x y) (* x y))
  (accumulate mult 1 term a next b))  


;; 1.33
(define (filtered-accumulate combiner null-value term a next b predicate?)
  (cond ((> a b) null-value)
        ((predicate? a)
         (combiner (term a)
                   (accumulate combiner null-value term (next a) next b)))
        (accumulate combiner null-value term (next a) next b)))
;; 1.33.a
(filtered-accumulate add 0 square a inc b prime?)
 
;; 1.33.b ???

;; 1.34
(define (f g)
  (g 2))
(f square)
(f (lambda (z) (* z (+ z 1))))
(f f) ;; wrong argument...
      ;; procedure application: expected procedure, given: 2; arguments were: 2

;; 1.34.5
  
