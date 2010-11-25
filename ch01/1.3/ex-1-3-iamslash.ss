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

;; 1.35
(define tolerance 0.00001)  
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (sqrt x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

(fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0) ;; 1.6180327868852458

;; 1.36

(define (fixed-point f first-guess)
  (define tolerance 0.00001)  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
(fixed-point (lambda (x)
               (/ (+ x (/ (log 1000) (log x)))) 2) 2)

;; 1.37

(define (cont-frac n d k)
  (define (cont-frac-inter n d k i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-inter n d k (+ 1 i))))))        
  (cont-frac-inter n d k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)
;; 1.38


;; 1.40
(define (fixed-point f first-guess)
  (define tolerance 0.00001)  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (cube x)
  (* x x x))
(define (square x)
  (* x x))
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* (+ b 1) x) c)))
(newtons-method (cubic a b c) 1)
(newtons-method (cubic 2 3 4) 1)

;; 1.41
(define (double f)
  (lambda (x)
    (f (f x))))
(define (inc n)
  (+ n 1))
((double inc) 5)           ;; 7
(((double double) inc) 5)  ;; 9
(((double (double double)) inc) 5) ;; 21

;; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (square x)
  (* x x))
(define (inc x)
  (+ x 1))
((compose square inc) 6)

;; 1.43
(define (compose f g)
  (lambda (x)
    (f (g x))))
;; (define (repeated f n)
;;   (define (iter n r)
;;     (if (= n 1) r
;;         (iter (- n 1)
;;               (compose f r))))
;;   (iter n f))
(define (repeated f n)
  (if (= n 1) f
      (repeated (compose f f) (- n 1))))
(define (square x)
  (* x x))
((repeated square 2) 5)

;; 1.44
(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (repeated f n)
  (if (= n 1) f
      (repeated (compose f f) (- n 1))))
(define dx 0.00001)
(define (average a b c)
  (/ (+ a b c) 3.0))

(define (smooth f)
  (lambda (x)
    (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (smooth-n f n)
  ((repeated smooth n) f))

(define (inc n)
  (+ n 1))
((smooth inc) 1)
((smooth (smooth inc)) 1)
((smooth-n inc 2) 1)

;; 1.45
(define (fixed-point f first-guess)
  (define tolerance 0.00001)  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (average x y)
  (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (repeated f n)
  (if (= n 1) f
      (repeated (compose f f) (- n 1))))
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)) 1.0)))
(define (n-rt x n)
  (define (exp_ x n)
    (if (= n 1) x
        (* x (exp_ x (- n 1)))))        
  (define (f x)
    (lambda (y) (/ x (exp_ y (- n 1)))))
  (fixed-point ((repeated average-damp n) f) 1.0))
(define (inc n)
  (+ n 1))
;;(n-rt 4 2)