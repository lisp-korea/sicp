

;; 1.1 프로그램을 짤 때 바탕이 되는 것

;; 기본식 primitive expression - 언어에서 가장 단순한 것을 나타낸다.
;; 엵어내는 수단 means of combination - 간단한 것을 모아 복잡한 것 compound element 으로 만든다
;; 요약하는 수단 means of abstraction - 복잡한 것에 이름을 붙여 하나로 다룰 수 있게끔 간추린다.


;; 1.1.1 식

486

;; 수를 나타내는 식과,  기본 프로시저를 나타내는 식 +,나 * 같은 기호를 엵은 더 복잡한 식
(+ 137 349)

(- 1000 334)

(* 5 99)

(/ 10 5)

(+ 2.7 10)

;; 위와 같이 여러 식을 괄호로 묶어 리스트를 만들고 프로시저 적용을 뜻하도록 엵어놓은 식을
;; 엵은 식이라고 한다. 이 리스트에서 맨 왼쪽에 있는 식은 연산자Operator 가 되고 나머지 식은
;; 피 연산자Operand 가 된다.


;; 1.1.2 이름과 환경

(define size 2)

size

(* 5 size)

(define pi 3.14159)

(define radius 10)

(* pi (* radius radius))

(define circumference (* 2 pi radius))


;; 어떤 값에 이름symbol 을 붙여 두었다가 뒤에 그 이름으로 필요한 값을 찾아
;; 쓸 수 있다는 것은, 실행기 속 어딘가에 이름-물체 의 쌍을 저장해둔 메모리가 있다는 뜻이다.
;; 이런 기억공간을 환경Environment 라고 한다.
;; 지금 여기서 '환경' 이란 맨 바깥쪽에 있는 바탕환경global environment 을 말한다

;; 1.1.3 엵은 식을 계산하는 방법

;; 1. 엵은 식에서 부분식subexpression 의 값을 모두 구한다
;; 2. 엵은 식에서 맨 왼쪽에 있는 식(연산자) 의 값은 프로시저가 되고, 나머지 식(피연산자) 의 값은
;;    인자가 된다. 프로시저를 인자에 적용하여 엵은 식의 값을 구한다.


;; 엵은 식의 값을 셈하는 프로세스를 끝내려면 부분식부터 계산해야 하는데, 부분식의 값을 셈할 때에도
;; 똑같은 프로세슬 따르도록 하고 있다.

(* (+ 2 (* 4 6))
   (+ 3 5 7))


;; 1.1.4 묽음 프로시저

;; 1. 수와 산술 연산이 기본 데이터이고 기본 프로시저이다.
;; 2. 엵은 식을 겹쳐 쓰는 것이 여러 연산을 한데 묽는 수단이 된다.
;; 3. 이름과 값을 짝지워 정의한 것이 모자라나마 요약하는 수단이 된다.


;; 프로시저를 어떻게 정의하는가

;;제곱

(define (square x)
  (* x x))

(square 21)
(square (+ 2 5))
(square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)


;; 1.1.4 맞바꿈 계산법으로 프로시저를 실행하는 방법

;; 기본프로시저를 계산 하는 방법은 이미 실행기 속에 정해져 있다고 보고
;; 새로 만들어 쓰는 묽음 프로시저의 적용은 다음 규칙에 따란 계산 된다.
;;  - 묽음 프로시저를 인자에 맞춘다는 것은, 프로시저의 몸속에 있는 모든 인자이름을 저마다
;;    그에 대응하는 인자값으로 맞바꾼 다음, 그렇게 얻어낸 식의 값을 구하는 것이다

(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square 6) (square 10))

(+ (* 6 6) (+ 10 10))



;; 인자 먼저 계산법 과 정의대로 계산법

;; 정의대로 계산법
(sum-of-squares (+ 5 1) (* 5 2))

(+ (square (+ 5 1)) (square (* 5 2)))

(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))


(+ (* 6 6) (* 10 10))

(+ 36 100)

136



;; 1.1.6 조건식과 술어

(define (abs x)
  (cond ((> x 0) x)   
	((= x 0) 0)
	((< x 0) (- x))))


(define (abs x)
  (cond ((< x 0) (- x))
	(else x)))


(and (= 10 10) (= 20 20) (= 30 30))
(and (= 10 10) (= 30 30) (* 20 20))

(or (= 10 20) (= 30 20))
(or (= 10 10) (= 30 20))
(or (= 10 20) (+ 100 200) (= 30 20))




;; 연습문제 1.1

10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))


;; 연습문제 1.2

5 + 4 + (2 - (3 - (6 + 4/5)))
-----------------------------
      3(6 - 2)(2 - 7)


(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5.0))))) (* 3 (- 6 2) (- 2 7)))

;; 연습문제 1.3
;; 세 숫자를 인자로 받아 그 가운데 큰 숫자 두 개를 제곱한 다음, 그 다 값을 덧셈하여 내놓는 프로시저를 정의하라

(define (square-of-two-bignum x y z)
  (if (>= x y) (if (>= y z) (+ (square x) (square y))
		   (+ (square x) (square z)))
      (if (>= x z) (+ (square y) (square x))
	  (+ (square y) (square z)))))


;; 연습문제 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 10 -20)

((if (> -20 0) + -) 10 -20)

(- 10 -20)


;; 연습문제 1.5
;; 인자 먼저 / 혹은 제때 계산하는 실행기에 따라 어떻게 다른가.
(define (p)
  (p))

(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p)) ;; 인자먼저 계산시 test 함수body 에 진입하기 전에 (p) 에서 무한루프



;; 1.1.7 연습 : 뉴튼 법 으로 제곱근 찾기



(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


;; 연습문제 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))


;; 연습문제 1.7

;; 위의 good-enough? 으로는 아주 작은 수의 제곱근을 구하지 못한다.

(sqrt 0.00001)
(sqrt-iter 1 0.00001)

;; 이에 따라 guess 를 구하기 위해 어림잡은 값을 조금씩 고쳐나가면서 헌값에 견주어 고친값이
;; 그다지 나아지지 않을 때깨지 계산을 이어나가자.

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) 0.001))

;; 연습문제 1.8
;; 세제곱근을 구해보자. 뉴튼의 세제곱근 공식:

x / y의 제곱 + 2y
----------------    =    (/ (+ (/ x (square y)) (* 2 y)) 3)
        3

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(sqrt-iter 1.0 8)
(sqrt-iter 1.0 27)
(sqrt-iter 1.0 64)
(sqrt-iter 1.0 125)


;; 1.1.8 블랙박스처럼 간추린 프로시저 / 갇힌 이름 / 안쪽 정의와 블록 구조

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))



