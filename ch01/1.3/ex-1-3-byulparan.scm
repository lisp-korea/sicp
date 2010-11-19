;; 연습문제 1.29

(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))



(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (y k) (cond ((or (= k 0) (= k n)) 1)
		   ((odd? k) 4)
		   (else 2))))
  (* (/ h 3.0) (sum term 0 inc n)))


(simpson cube 0 1 100)
;=> 0.25
(simpson cube 0 1 1000)
;=> 0.25



;; 연습문제 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))


(simpson cube 0 1 100)
;=> 0.25
(simpson cube 0 1 1000)
;=> 0.25



;; 연습문제 1.31
;; a.
(define (product f a next b)  ;; recursive
  (if (> a b)
      1
      (* (f a) (product f (next a) next b))))

(define (my-pi n)  
  (define (term k)
    (/ (+ k (if (odd? k) 1 2))
       (+ k (if (odd? k) 2 1))))
  (* 4.0 (product term 1 inc n)))

(my-pi 100)
;=> 3.1570301764551676
(my-pi 1000)
;=> 3.1431607055322663

;; b. iterate
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (my-pi n)  
  (define (term k)
    (/ (+ k (if (odd? k) 1 2))
       (+ k (if (odd? k) 2 1))))
  (* 4.0 (product-iter term 1 inc n)))


(my-pi 100)
;=> 3.1570301764551676
(my-pi 1000)
;=> 3.1431607055322663

;; 연습문제 1.32
;; a. recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))
;=> (sum cube 0 inc 10)

(define (product term a next b)
  (accumulate * 1 term a next b))
;=> (product cube 1 inc 10)

;; b. iter
(define (accumulate-iter combiner null-value term a next b)
  (define (iter k result)
    (if (> k b)
	result
	(iter (next k) (combiner result (term k)))))
  (iter a null-value))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(= (product-iter cube 1 inc 10) (product cube 1 inc 10))
;=> #t


;; 연습문제  1.33

(define (filtered-accumulate combiner null-value term a next b pred)
  (define (iter k result)
    (if (> k b)
	result
	(cond ((pred k) (iter (next k) (combiner result (term k))))
	      (else (iter (next k) result)))))
  (iter a null-value))

;; a
(define (square x)
  (* x x))

(define (prime? n)
  (define (smallest-divisor)
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (find-divisor test-divisor)
      (cond ((> (square test-divisor) n) n)
	    ((divides? test-divisor n) test-divisor)
	    (else (find-divisor (+ test-divisor 1)))))
    (find-divisor 2))
  (= n (smallest-divisor)))

(define (sum-squared-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(= (sum-squared-prime 0 10) (+ (square 0) (square 1) (square 2) (square 3)
			    (square 5) (square 7)))
;=> #t

;; b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (filtered-test n)
  (define (value x)
    x)
  (define (gcd-test? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 value 0 inc n gcd-test?))


(= (filtered-test 10) (* 1 3 7 9))
;=> #t


;; 연습문제 1.34

(define (f g)
  (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))

(f f)
(f 2)
(2 2) ;;??
;; f 프로시져는 프로시져를 인자로 받아야 하지만 결국 2 라는 숫자를 받게 됨.



;; 본문 1.3.3 일반적인 방법을 표현하는 프로시저

(define (search f neg-point pos-point)
  (define (average a b)
    (/ (+ a b) 2.0))
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value) (search f neg-point midpoint))
		((negative? test-value) (search f midpoint pos-point))
		(else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
	  ((and (negative? b-value) (positive? a-value)) (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

;; f(x) = x - 5
;; f(2) = -3
;; f(7) = 2
(half-interval-method (lambda (x) (- x 5)) 2 7)
;=> 5.00018310546875
(let ((f (lambda (x) (- x 5))))
  (f (half-interval-method f 2 7)))


(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)
;=> 1.89306640625
(let ((f (lambda (x) (- (* x x x) (* 2 x) 3))))
  (f (half-interval-method f 1.0 2.0)))


;;; 함수의 고정점 찾긔

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

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (average a b)
  (/ (+ a b) 2.0))

(define (my-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))


;; 연습문제 1.35

(define golden-ratio 1.6180)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;=> 1.6180327868852458

(> tolerance (abs (- golden-ratio ((lambda (x) (+ 1 (/ 1 x))) golden-ratio))))
;=> #f -_-;;

(let ((golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)))
  (> tolerance (abs (- golden-ratio ((lambda (x) (+ 1 (/ 1 x))) golden-ratio)))))
;=> #t


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; http://wiki.drewhess.com/wiki/SICP_exercise_1.35
;;
;; f(x) = x 일때 x 가 고정점이라면...
;; 1 + 1 / x = x  일때...x 가 고정점.
;; x^2 = x + 1
;; x^2 - x - 1 = 0
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(half-interval-method (lambda (x) (- (* x x) x 1)) 1 5)
;=> 1.61767578125
(half-interval-method (lambda (x) (- (* x x) x 1)) 1 10)
;=> 1.618255615234375
(half-interval-method (lambda (x) (- (* x x) x 1)) 1 100)
;=> 1.6182212829589844


;; 연습문제 1.36

(define (fixed-point2 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

; x |-> log(1000) / log(x)

(fixed-point2 (lambda (x) (/ (log 1000) (log x))) 10.0)
; 약 34단계
;=> 4.555532257016376
(expt 4.555532257016376 4.555532257016376)
;=> 999.991323238027

(define (average a b)
  (/ (+ a b) 2.0))

(fixed-point2 (lambda (x) (average x (/ (log 1000) (log x))))
	      10.0)
;=> 11단계
;=> 4.555536206185039


;; 연습문제 1.37
;; a.

;; (/ (n 1) (+ (d 1) (/ (n 2) (+ (d 2) (/ (n 3) (+ (d 3) (/ (n 4) ... (+ (/ (n k) (d k))))))))))

(define golden-ratio 1.6180)

(define (cont-frac n d k)
  (define (cont-frac-inner i)
    (if (> i k)
	(/ (n i) (d i))
	(/ (n 1) (+ (d i) (cont-frac-inner (+ i 1))))))
  (cont-frac-inner 1))

(define (cont-frac-count)
  (define (cont-frac-count-inner count)
    (if (> tolerance (abs (- (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) count) (/ 1 golden-ratio))))
	count
	(cont-frac-count-inner (+ count 1))))
  (cont-frac-count-inner 1))

(cont-frac-count)
;=> 10

(abs
 (- (/ 1 golden-ratio)
    (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)))
;=> 8.58398571634833e-06

;; b.

(define (cont-frac-iter n d k)
  (define (cont-frac-iter-inner i result)
    (if (= i 0)			
	result
	(cont-frac-iter-inner (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-iter-inner k (/ (n k) (d k))))

(= 
 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)
 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10))

;; 연습문제 1.38



;; 연습문제 1.39

;; tan x = (/ x (- 1 (/ (* x x) (- 3 (/ (* x x) (- 5 (/ (* x x) (- 7 (/ (* x x)   .....  (/ (* x x) (+ (- k 1) k))

(define (tan-cf x k)
  (let ((n (lambda (i) (if (= i 1) x (- (* x x)))))
	(d (lambda (i) (- (* 2 i) 1))))
    (cont-frac-iter n d k)))

(define (between a b)
  (> tolerance (abs (- a b))))

(between
 (tan pi)
 (tan-cf pi 20))
;=> #t

(between
 (tan 10.0)
 (tan-cf 10.0 20))
;=> #t