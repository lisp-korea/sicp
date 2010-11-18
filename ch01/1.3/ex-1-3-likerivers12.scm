;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 1.3 차수 높은 프로시저(higher-order procedure)로 요약하는 방법
;;; p72

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p72
(define (cube x) (* x x x))

(* 3 3 3)
(* x x x)
(* y y y)



;;;;=================================<ch 1.3.1>=================================
;;; 프로시저를 인자로 받는 프로시저
;;; p73

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p73
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))


;;;->
;; (define (<name> a b)
;;   (if (> a b)
;;       0
;;       (+ (<term> a)
;; 	 (<name> (<next a) b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p75~76
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)
;;->3025

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(integral cube 0 1 0.0001)
(integral cube 0 1 0.00001)
(integral cube 0 1 0.000001)

;;;; sine 함수 적분 테스트
(integral sin 0 (/ 3.14159 2) 0.001)

(integral identity 0 1 0.001)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 가우시안 함수 적분 테스트

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (gaussian m sigma x)
  (define PI 3.14159)
  (define EXP 2.7183)
  (/ (expt EXP (/ (- (square (- x m)) ) (* 2 (square sigma)))) (sqrt (* 2 PI (square sigma)))))

;;; 1-D Gaussian function generator
(define (gen-gaussian m sigma)
  (let ((PI 3.14159)
	(E 2.7183))
    (lambda (x)
      (/ (expt E 
	       (/ (- (expt (- x m) 2)) (* 2 (expt sigma 2))))
	 (sqrt (* 2 PI (expt sigma 2)))))))

(define gaussian-f (gen-gaussian 0 1))

(integral gaussian-f -100 100 0.01)




;;;--------------------------< ex 1.29 >--------------------------
;;; p77

;;; 앞의 방식으로 cube 정적분
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)




;;; Simpson 방법으로 cube 정적분
;; h/3 [ y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 2yn-2 + 4yn-1 + yn]
;; => h/3 [ [y0 + yn] +
;;          4[y1 + y3 + ... + yn-3 + yn-1] +
;;          2[y2 + y4 + ... + yn-2 + yn] ]

(define (inc2 n)
  (+ n 2))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk k)
    (f (+ a (* k h))))
  (* (/ h 3)
	(+ (yk 0)
	   (yk n)
	   (* 4 (sum yk 1 inc2 (- n 1)))
	   (* 2 (sum yk 2 inc2 n)))))

(integral-simpson cube 0 1.0 100)
(integral-simpson cube 0 1.0 1000)
(integral-simpson cube 0 1.0 10000)
(integral-simpson cube 0 1.0 100000)
(integral-simpson cube 0 1.0 1000000)
;; 원래 0.25 임 

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(integral cube 0 1 0.0001)
(integral cube 0 1 0.00001)
(integral cube 0 1 0.000001)
;; 원래 0.25 임.

(integral-simpson gaussian-f -100 100 100)
(integral-simpson gaussian-f -100 100 1000)
(integral-simpson gaussian-f -100 100 10000)
(integral-simpson gaussian-f -100 100 100000)
;; 원래 거의 1임




;;;--------------------------< ex 1.30 >--------------------------
;;; p77
;;; sum의 선형재귀프로세스(linear recursion process) 

;; (define (sum term a next b)
;;   (define (iter a result)
;;     (if <>
;; 	<>
;; 	(iter <> <>)))
;;   (iter <> <>))


(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc n)
  (+ n 1))

(sum-iter identity 1 inc 10)

;;;------
(define (integral-iter f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum-iter f (+ a (/ dx 2.0)) add-dx b)
     dx))


(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(integral cube 0 1 0.0001)
(integral cube 0 1 0.00001)
(integral cube 0 1 0.000001)
;; 원래 0.25 임.


(integral-iter cube 0 1 0.01)
(integral-iter cube 0 1 0.001)
(integral-iter cube 0 1 0.0001)
(integral-iter cube 0 1 0.00001)
(integral-iter cube 0 1 0.000001)
;; 원래 0.25 임.

;;;------



;;;--------------------------< ex 1.31 >--------------------------
;;; p78

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))


(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))


;;;-----
(define (inc2 n)
  (+ n 2))
;;;-----

;; pi/4 = 2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7 ...
;;  =>  2/3 * 4/5 * 6/7 * ...
;;    * 4/3 * 6/5 * 8/7 * ...

;;; 되도는 product 이용
(define (find-pi b)
  (define (term1 x)
    (/ x (+ x 1)))
  (define (term2 x)
    (/ (+ x 2) (+ x 1)))
  (* (product term1 2 inc2 b)
     (product term2 2 inc2 b)
     4.0))


(find-pi 10)
(find-pi 100)
(find-pi 1000)
(find-pi 10000)
(find-pi 100000)

;;;----------------------------
;;; 반복 product 이용
(define (find-pi2 b)
  (define (term1 x)
    (/ x (+ x 1)))
  (define (term2 x)
    (/ (+ x 2) (+ x 1)))
  (* (product-iter term1 2 inc2 b)
     (product-iter term2 2 inc2 b)
     4.0))


(find-pi2 10)
(find-pi2 100)
(find-pi2 1000)
(find-pi2 10000)
(find-pi2 100000)



;;;--------------------------< ex 1.32 >--------------------------
;;; p78

;;;-------
(define (inc n) (+ n 1))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))
;;;-------



;;; 되도는 accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))


(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(sum identity 1 inc 10)
(sum-acc identity 1 inc 10)


(define (product-acc term a next b)
  (accumulate * 1 term a next b))

(product identity 1 inc 10)
(product-acc identity 1 inc 10)



;;; 반복 accumulate
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) 
	      (combiner result (term a)))))
  (iter a null-value))


(define (sum-acc-iter term a next b)
  (accumulate-iter + 0 term a next b))

(sum identity 1 inc 10)
(sum-acc identity 1 inc 10)
(sum-acc-iter identity 1 inc 10)


(define (product-acc-iter term a next b)
  (accumulate-iter * 1 term a next b))

(product identity 1 inc 10)
(product-acc identity 1 inc 10)
(product-acc-iter identity 1 inc 10)




;;;--------------------------< ex 1.33 >--------------------------
;;; p79

;;; 되도는 filtered-accumulate
(define (filtered-accumulate predicator combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (predicator a)
	  (combiner (term a)
		    (filtered-accumulate predicator
					 combiner 
					 null-value term (next a) next b))
	  (combiner null-value
		    (filtered-accumulate predicator
					 combiner 
					 null-value term (next a) next b)))))
;;; 반복 filtered-accumulate
(define (filtered-accumulate-iter predicator combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(if (predicator a)
	    (iter (next a) (combiner result (term a)))
	    (iter (next a) result))))
  (iter a null-value))


(define (square x)
  (* x x))

(define (sum-of-prime a b)
  (filtered-accumulate prime? + 0 identity a inc b))

(sum-of-prime 1 10)  ; 1은 소수가 아니다.
(+ 2 3 5 7)

;;; a------------------------------------------
(define (sum-of-square-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (sum-of-square-prime-iter a b)
  (filtered-accumulate-iter prime? + 0 square a inc b))

(sum-of-square-prime 2 10)
(sum-of-square-prime-iter 2 10)
(+ (* 2 2) (* 3 3) (* 5 5) (* 7 7)) 


;;; b------------------------------------------
(define (product-of-relative-prime a b)
  (define (relative-prime? i)
    (if (= (gcd i b) 1)
	#t
	#f))
  (filtered-accumulate relative-prime? * 1 identity a inc b))

(define (product-of-relative-prime-iter a b)
  (define (relative-prime? i)
    (if (= (gcd i b) 1)
	#t
	#f))
  (filtered-accumulate-iter relative-prime? * 1 identity a inc b))

(product-of-relative-prime 1 15)
(product-of-relative-prime-iter 1 15)
;;; relative primes to 15.
;;; 2 4 7 8 11 13 14
(* 2 4 7 8 11 13 14)

;;; for test
(define (test-relative-prime? i b)
  (define (relative-prime? i)
    (if (= (gcd i b) 1)
	#t
	#f))
  (relative-prime? i))

(test-relative-prime? 2 15) ;t
(test-relative-prime? 3 15)
(test-relative-prime? 4 15) ;t
(test-relative-prime? 5 15)
(test-relative-prime? 6 15)
(test-relative-prime? 7 15) ;t
(test-relative-prime? 8 15) ;t
(test-relative-prime? 9 15)
(test-relative-prime? 10 15)
(test-relative-prime? 11 15) ;t
(test-relative-prime? 12 15)
(test-relative-prime? 13 15) ;t
(test-relative-prime? 14 15) ;t


;;;-----------
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;----
(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (<= n 1)
      #f      ; 1은 소수가 아니다.
      (= n (smallest-divisor n))))
;;;------------





;;;;=================================<ch 1.3.2>=================================
;;; lambda로 나타내는 프로시저
;;; p79

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p80
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(pi-sum 1 10)


(define (integral f a b dx)
  (* (sum f
	  (+ a (/ dx 2.0))
	  (lambda (x) (+ x dx))
	  b)
     dx))

(integral (lambda (x) x) 0 1 0.01)



;;; 아래 둘은 같은 것이다.
(define (plus4 x) (+ x 4))

(define plus4 (lambda (x) (+ x 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p81

;; 프로시저 이름이 들어갈 수 있는 모든 자리에 lambda 식을 써도 좋다

((lambda (x y z) (+ x y (square z))) 1 2 3)




;;;;------------------------------------------------------------------
;;; let으로 갖힌 변수 만들기


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p82
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
	    (- 1 y)))

(f 3 4)

;;=> lambda 식으로
(define (f x y)
  ((lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(f 3 4)


;;=> let을 써서
(define (f x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(f 3 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p83

;;; let식은 lambda 식을 더 편하게 쓰려고 만든 달콤한 문법일 뿐이다.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p84

(let ((x 5))
  (+ (let ((x 3))
       (+ x (* x 10)))
     x))

;;; define으로 안쪽에서 이름을 정의하여 let처럼 쓸 수 있다.
;;; 하지만 이 책에서는 앞으로 define은 안쪽에서 프로시저를 정의할 때에만 쓰기로 한다.

(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))

(f 3 4)



;;;--------------------------< ex 1.34 >--------------------------
;;; p85

;;;---
(define (square x)
  (* x x))
;;;---

(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

;;; 다음은 어떻게 되는가? 왜 그런가?
(f f)
;;-> (f 2)
;; f에 숫자 2를 적용하는 방법이 정의되어 있지 않다.





;;;;=================================<ch 1.3.3>=================================
;;; 일반적인 방법을 표현하는 프로시저
;;; p85

;;;;------------------------------------------------------------------
;;; 이분법으로 방정식의 근 찾기

;;; p86

(define (average a b)
  (/ (+ a b) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p86
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite" a b)))))

;;; sin(x) =0 의 근 찾기
(half-interval-method sin 2.0 4.0)

;;; x^3 - 2x - 3 = 0 의 실근
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
		      1.0
		      2.0)



;;;;------------------------------------------------------------------
;;; 함수의 고정점 찾기
;;; p88
;;; f(x) = x 가 참이면 x를 f의 고정점(fixed point)라 한다.

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

;;; p88

(fixed-point (lambda (y) (+ (sin y) (cos y)))
	     1.0)


;;; x의 제곱근은 y^2=x 라는 조건에 맞는 y를 찾는 문제이다.
;;; -> y=x/y => y |-> x/y 함수의 고정점을 찾는 문제와 같다

(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
	       1.0))

(sqrt 2)
;;<- 답이 안나옴.

;;; 평균 내어 잠재우기(average damping)
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))

(sqrt 2)


;;;--------------------------< ex 1.35 >--------------------------
;;; p90

;;; in p50
;;; phi = (1+sqrt(5))/2 =~ 1.6180

;;; phi^2 = phi + 1
;;; => phi = 1 + 1/phi
;;; => x |-> 1 + 1/x

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;;-> 1.6180327868852458



;;;--------------------------< ex 1.36 >--------------------------
;;; p90

;;;---
(define tolerance 0.00001)
;;;---

(define (fixed-point-display f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (print guess)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point-display (lambda (x) (/ (log 1000) (log x)))
		     1.5)
;;;-> 4.555539351985717



;;;--------------------------< ex 1.37 >--------------------------
;;; p90,91

;;; 되돌기 프로세스(recursive process)
(define (cont-frac n d k)
  (define (jth-frac j)
    (if (= j k)
	(/ (n j) (d j))
	(/ (n j) (+ (d j) (jth-frac (+ j 1))))))
  (jth-frac 1))

;;; phi =~ (1 + (sqrt 5)) / 2 =~ 1.618033988749989
;;; 1/phi =~ 0.6180339887498588

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1)  ; 1.0
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 2)  ; 0.5
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 3)  ; 0.6666666666666666
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 4)  ; 0.6000000000000001
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5)  ; 0.625
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)  ; 0.6179775280898876
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 15)  ; 0.6180344478216819
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 20)  ; 0.6180339850173578
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 30)  ; 0.6180339887496482
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 50)  ; 0.6180339887498948

;;; test 
(cont-frac (lambda (i) 1.0) (lambda (i) 2.0) 3)  ; (/ 1. (+ 2 (/ 1 (+ 2 (/ 1 2)))))



;;; 반복 프로세스(iterative process)
(define (cont-frac-iter n d k)
  (define (frac-inner j acc-frac)
    (if (> j 2)
	(frac-inner (- j 1)
		     (/ (n (- j 1))
			(+ (d (- j 1)) acc-frac)))
	(/ (n 1) (+ (d 1) acc-frac))))
  (if (= k 1)
      (/ (n 1) (d 1))
      (frac-inner k (/ (n k) (d k)))))

(/ 1. (+ 1 (/ 1 (+ 1 (/ 1 1)))))  ; k:3
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 1)   ; 1.0
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 2)   ; 0.5
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 3)   ; 0.6666666666666666
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 4)   ; 0.6000000000000001
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 5)   ; 0.625
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)  ; 0.6179775280898876
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 15)  ; 0.6180344478216819
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 20)  ; 0.6180339850173578
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 30)  ; 0.6180339887496482
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 50)  ; 0.6180339887498948



;;;--------------------------< ex 1.38 >--------------------------
;;; p91,92

;; 1/
;;   (1 + 1/
;;     (2 + 1/
;;       (1 + 1/
;;         (1 + 1/
;;           (4 + 1/
;;             (1 + 1/
;;                ...
;;                       ))))))

;; j mod 3
;; ->   1 2 0
;; -----------
;; dj : 1 2 1     (j: 1  2  3)
;;      1 4 1     (   4  5  6)
;;      1 6 1     (   7  8  9)
;;      1 8 1 ... (  10 11 12)
;;        ^
;;        | : (j + 1) / 3 * 2

(define (cont-frac-euler k)
  (define (d-euler j)
    (let ((rem (remainder j 3)))
      (cond ((= rem 0) 1.0)
	    ((= rem 1) 1.0)
	    (else (* (/ (+ j 1.) 3.) 2.0)))))
  (cont-frac-iter (lambda (i) 1.0) d-euler k))


;; e =~ 2.718281828459045
;; <- (exp 1)
;; e-2 =~ 0.718281828459045
(cont-frac-euler 1) ; 1.0                  ; (/ 1. 1)
(cont-frac-euler 2) ; 0.6666666666666666   ; (/ 1. (+ 1 (/ 1 2)))
(cont-frac-euler 3) ; 0.75                 ; (/ 1. (+ 1 (/ 1 (+ 2 (/ 1 1)))))
(cont-frac-euler 4) ; 0.7142857142857143
(cont-frac-euler 5) ; 0.71875 ; (/ 1. (+ 1 (/ 1 (+ 2 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 4)))))))))
(cont-frac-euler 6) ; 0.717948717948718
(cont-frac-euler 10)
(cont-frac-euler 20) ; 0.7182818284590452
(cont-frac-euler 30) ; 0.7182818284590453
(cont-frac-euler 40) ; 0.7182818284590453
(cont-frac-euler 50) ; 0.7182818284590453

;(/ 1. (+ 1 (/ 1 (+ 2 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 4 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 6 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 8 (/ 1 1)))))))))))))))))))))))
;-> 0.7182818229439497


;;;--------------------------< ex 1.39 >--------------------------
;;; p92

;;; 되돌기 프로세스(recursive process)
(define (tan-cf x k)
  (define (n i)
    (square i))
  (define (d i)
    (+ (* (- i 1) 2) 1))
  (define (tan-cf-rec n d k)
    (define (jth-frac j)
      (if (= j k)
	  (/ (n x) (d j))
	  (/ (n x) (- (d j) (jth-frac (+ j 1))))))
    (jth-frac 2))
  (if (= k 1)
      (/ x (d 1))
      (/ x (- (d 1) (tan-cf-rec n d k)))))

(tan 1) ; 1.5574077246549023

(tan-cf 1. 1)  ; 1.0
(tan-cf 1. 2)  ; 1.4999999999999998
(tan-cf 1. 3)  ; 1.5555555555555558
(tan-cf 1. 4)  ; 1.5573770491803278
(tan-cf 1. 5)  ; 1.5574074074074076
(tan-cf 1. 10) ; 1.557407724654902
(tan-cf 1. 15) ; 1.557407724654902
(tan-cf 1. 20) ; 1.557407724654902

;;; 반복 프로세스
(define (tan-cf x k)
  (define (tan-cf-iter n1 n d k)
    (define (jth-tan-cf-inner j acc-frac)
      (if (> j 2)
	  (jth-tan-cf-inner (- j 1)
			    (/ (n x)
			       (- (d (- j 1)) acc-frac)))
	  (/ (n1 x) (- (d 1) acc-frac))))
    (if (= k 1)
	(/ (n x) (d 1))
	(jth-tan-cf-inner k (/ (n x) (d k)))))
  (tan-cf-iter (lambda (i) i) 
	       (lambda (i) (square i))
	       (lambda (i) (+ (* (- i 1) 2) 1))
	       k))

(tan 1) ; 1.5574077246549023

(tan-cf 1. 1)  ; 1.0
(tan-cf 1. 2)  ; 1.4999999999999998
(tan-cf 1. 3)  ; 1.5555555555555558
(tan-cf 1. 4)  ; 1.5573770491803278
(tan-cf 1. 5)  ; 1.5574074074074076
(tan-cf 1. 10) ; 1.557407724654902
(tan-cf 1. 15) ; 1.557407724654902
(tan-cf 1. 20) ; 1.557407724654902


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 반복 프로세스
(define (tan-cf x k)
  (define (tan-cf-iter n1 n d k)
    (define (tan-cf-inner j acc-frac)
      (if (> j 2)
	  (tan-cf-inner (- j 1)
			(/ (n x)
			   (- (d (- j 1)) acc-frac)))
	  (/ (n1 x) (- (d (- j 1)) acc-frac))))
    (if (= k 1)
	(/ (n x) (d 1))
	(tan-cf-inner k (/ (n x) (d k)))))
  (tan-cf-iter (lambda (i) i) 
	       (lambda (i) (square i))
	       (lambda (i) (+ (* (- i 1) 2) 1))
	       k))

(tan 1) ; 1.5574077246549023

(tan-cf 1. 1)  ; 1.0
(tan-cf 1. 2)  ; 1.4999999999999998
(tan-cf 1. 3)  ; 1.5555555555555558
(tan-cf 1. 4)  ; 1.5573770491803278
(tan-cf 1. 5)  ; 1.5574074074074076
(tan-cf 1. 10) ; 1.557407724654902
(tan-cf 1. 15) ; 1.557407724654902
(tan-cf 1. 20) ; 1.557407724654902




;;; 아래와 같은 형태를 갖는 연속분수 계산함수를 만들어내는 함수
; n1(x) /
;        C( d(1) , n(x) /
;                         C( d(2) , n(x) / 
;                                         ...
;                                         C( d(k-1) , n(x) / d(k)

;;; 반복 프로세스(iterative process)
(define (cf-general-iter combiner n1 n d)
  (lambda (x k)
    (define (frac-inner j acc-frac)
      (if (> j 2)
	  (frac-inner (- j 1)
		      (/ (n x)
			 (combiner (d (- j 1)) acc-frac)))
	  (/ (n1 x) (combiner (d (- j 1)) acc-frac))))
    (if (= k 1)
	(/ (n x) (d 1))
	(frac-inner k (/ (n x) (d k))))))

(define tan-cf
  (cf-general-iter -
		   (lambda (i) i) 
		   (lambda (i) (square i))
		   (lambda (i) (+ (* (- i 1) 2) 1))))


(tan 1) ; 1.5574077246549023

(tan-cf 1. 1)  ; 1.0
(tan-cf 1. 2)  ; 1.4999999999999998
(tan-cf 1. 3)  ; 1.5555555555555558
(tan-cf 1. 4)  ; 1.5573770491803278
(tan-cf 1. 5)  ; 1.5574074074074076
(tan-cf 1. 10) ; 1.557407724654902
(tan-cf 1. 15) ; 1.557407724654902
(tan-cf 1. 20) ; 1.557407724654902





;;;;=================================<ch 1.3.4>=================================
;;; 프로시저를 만드는 프로시저
;;; p92


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p92

(define (average-damp f)
  (lambda (x) (average x (f x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p93

;;;---
(define (square x)
  (* x x))
;;;---

((average-damp square) 10)
;;55

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))
;; 고정점 찾기,  평균 잦아들기, y|-> x/y  구조가 그대로 드러남

(sqrt 2)


;;;;;;;;;;;;;;;;;;;;;;;
;;; p94

;;; y^3 = x
;;; y |-> x / y^2
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(cube-root 8)



;;;;------------------------------------------------------------------
;;; 뉴튼방법
;;; p94

;;; x |-> g(x) 가 미분되는 함수이면,
;;; g(x) = 0 의 근은 함수 x |-> f(x) 의 정점과 같다.
;;; 이 때, f(x) = x - g(x)/Dg(x)

;;; Dg(x) = (g(x + dx) - g(x)) / dx

;;;;;;;;;;;;;;;;;;;;;
;;; p95
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

((deriv cube) 5)



;;;---
(define (square x)
  (* x x))
;;;---

;;;; deriv를 써서 뉴턴 방법을 고정점 찾는 방법으로 표현
;;; f(x) = x - g(x)/Dg(x) 에 해당
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;; 제곱근 : y |-> y^2 - x       <-- ???
;;; y^2 = x   =>  y^2 - x = 0   <-- g(x) = 0 이라서?

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

(sqrt 2)


;;; 세제곱근을 뉴튼 방법과 고정점 찾는 방법으로
;;; y^3 = x  =>  y^3 - x = 0    <-- g(x) = 0   
(define (cube-root x)
  (newtons-method (lambda (y) (- (cube y) x))
		  1.0))

(cube-root 8)




;;;;------------------------------------------------------------------
;;; 요약과 일등급 프로시저
;;; p96

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))

(sqrt 2)
;-> 1.4142135623746899

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newton-transform
			    1.0))

(sqrt 2)
;-> 1.4142135623822438



;;;--------------------------< ex 1.40 >--------------------------
;;; p98


;;;--------------------------< ex 1.41 >--------------------------
;;; p98


;;;--------------------------< ex 1.42 >--------------------------
;;; p99


;;;--------------------------< ex 1.43 >--------------------------
;;; p99


;;;--------------------------< ex 1.44 >--------------------------
;;; p99


;;;--------------------------< ex 1.45 >--------------------------
;;; p100


;;;--------------------------< ex 1.46 >--------------------------
;;; p100

