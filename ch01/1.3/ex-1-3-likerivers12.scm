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




;;;--------------------------< ex 1.31 >--------------------------
;;; p78




;;;--------------------------< ex 1.32 >--------------------------
;;; p78





;;;--------------------------< ex 1.33 >--------------------------
;;; p79






;;;;=================================<ch 1.3.2>=================================
;;; lambda로 나타내는 프로시저
;;; p79







;;;;=================================<ch 1.3.3>=================================
;;; 일반적인 방법을 표현하는 프로시저
;;; p85






;;;;=================================<ch 1.3.4>=================================
;;; 프로시저를 만드는 프로시저
;;; p92

