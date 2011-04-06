;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 3 모듈, 물체, 상태
;;; Ch 3.5 스트림
;;; p411

;; 셈미룸 계산법(delayed evaluation" 기법을 쓰면 끝없는 차례열도 스트림으로 나타낼 수 있다.



;;;==========================================
;;; 3.5.1 스트림과 (계산을) 미룬 리스트
;;; p412

;; 차례열을 리스트로 표현하면 쉽고도 깔끔하기는 하지만,
;; 시간과 공간 효율이 모두 떨어지는 대가를 치러야 한다.

;; p414
;; 스트림은 리스트만큼 커다란 대가를 치르지 않으면서도 차례열 패러다임으로 프로그램을 짤 수 있도록 도와주는 멋진 기법이다.
;; 어떤 프로그램에서 스트림을 인자로 받는 경우.
;; - 스트림을 덜 만든 상태 그대로 넘긴 다음
;; - 스트림 자체가 딱 쓸 만큼만 알아서 원소를 만들어 내게끔 하는 것이다.

;; p415
(define the-empty-stream '())

(define (stream-null? s)
  (null? s))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;;-----------------------------------------------
;; p417
;; cons-stream 프로시저는 다음처럼 정의된 특별한 형태다.
;; (cons-stream <a> <b>)
;; ->
;; (cons <a> (delay <b>))
;;=> "다음처럼 정의된 특별한 형태다"라는 말은 define으로 정의하는 것이 아니라
;;   언어 해석기 차원에서 변환하도록 정의된다라는 말인 것 같다.
;;   즉 이것은 매크로로 정의하거나 언어 해석기의 구현을 이와 같이 해야한다.
;; (define (cons-stream a b) 
;;   (cons a (delay b)))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))


;;; 스트림은 어떻게 돌아가는가

;;------------------------------------------------
;;;---
;; p64 ch 1.2.6
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;;;---

;;; 어떤 범위에 있는 모든 정수를 뽑아내는 연산
(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

;; 일반 차례열
;; (filter prime?
;; 	(enumerate-interval 10000 1000000))
 

;;--------------------------------------------------
;; 스트림용 이누머레이터
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream 
       low 
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))


(cons 10000
      (delay (stream-enumerate-interval 10001 1000000)))

(stream-filter prime?
	       (stream-enumerate-interval 10000 1000000))
;;--------------------------------------------------


;; p420
;; 대개 셈미룸 계산법은 바라는 대로 프로그램이 실행되도록 하는 방식의 한 가지.

;;; delay와 force 프로시저 만들기

;; delay 프로시저는 아래와 같은 특별한 형태로 나타낸다.
;;                          ^^^^^^^^^^^^^^^^^
;;                       -> 'define으로 정의한다'가 아니라 매크로 또는 해석기의 구현을 의미
;; (delay <exp>)
;; ->
;; (lambda () <exp>)

;; (define (force delayed-object)
;;   (delayed-object))


;;; 셈미룬 물체를 처음으로 계산한 다음에 그 값을 어딘가에 적어 두었다가
;;; 나중에 그 물체를 다시 쓸 때 
;;; 같은 것을 계산한다면 적어둔 값을 쓰도록 한다.

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))

;;



;;;--------------------------< ex 3.50 >--------------------------
;;; p422

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

;;;--------------------------< ex 3.51 >--------------------------
;;; p422

(define x '())

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
	      (stream-enumerate-interval 0 10)))
;; 0

(stream-ref x 5)
;; 1;2;3;4;5,5

(stream-ref x 7)
;; 6;7,7

;;;--------------------------< ex 3.52 >--------------------------
;;; p423

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map accum
	      (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))
;; 
(define z (stream-filter (lambda (x) 
			   (= (remainder x 5) 0))
			 seq))

;; > sum
;; 1
;; > seq
;; (1 . #<promise>)
;; > > y
;; (6 . #<promise>)
;; > > z
;; (10 . #<promise>)

(stream-ref y 7)

(display-stream z)






;;;==========================================
;;; 3.5.2 무한 스트림(infinite stream)
;;; p424

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

integers 

(stream-ref integers 0)
(stream-ref integers 1)
(stream-ref integers 2)
(stream-ref integers 3)
(stream-ref integers 4)
(stream-ref integers 5)

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
		 integers))

(stream-ref no-sevens 100)


;; 피보나치 수들의 무한 스트림
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

fibs



;;; 에라토스테네스의 체
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

primes

(stream-ref primes 0)
(stream-ref primes 1)
(stream-ref primes 2)
(stream-ref primes 3)
(stream-ref primes 4)

(stream-ref primes 50)


;;;;;;;;;;;;;;;;;;;;;;;
;;; 스트림을 드러나지 않게 정의하는 방법
;;; p427

(define ones (cons-stream 1 ones))

ones
;; car는 1이고, cdr는 다시 ones를 계산하겠다는 약속

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

integers

(stream-ref integers 0)
(stream-ref integers 1)
(stream-ref integers 2)
(stream-ref integers 3)
(stream-ref integers 4)
(stream-ref integers 5)




;; 피보나치수열
(define fibs
  (cons-stream 0
	       (cons-stream 1
			    (add-streams (stream-cdr fibs)
					 fibs))))

fibs

(stream-ref fibs 0)
(stream-ref fibs 1)
(stream-ref fibs 2)
(stream-ref fibs 3)
(stream-ref fibs 4)
(stream-ref fibs 5)




;; 스트림의 모든 원소에 정해진 상수 값을 곱한다.
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

double


(stream-ref double 0)
(stream-ref double 1)
(stream-ref double 2)
(stream-ref double 3)
(stream-ref double 4)
(stream-ref double 5)



;; 정수의 스트림을 만든 후
;; 소수이면 걸러내는 방식
(define primes
  (cons-stream
   2
   (stream-filter prime?
		  (integers-starting-from 3))))


;; n이 sqrt(n) 과 같거나 그보다 작은 소수로 나누어지는지 따져봐야한다.
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t) ;true)
	  ((divisible? n (stream-car ps)) #f) ;false)
	  (else (iter (stream-cdr ps)))))
  (iter primes))

primes

(stream-ref primes 0)
(stream-ref primes 1)
(stream-ref primes 2)
(stream-ref primes 3)
(stream-ref primes 4)
(stream-ref primes 5)

;;;--------------------------< ex 3.53 >--------------------------
;;; p430

(define s (cons-stream 1 (add-streams s s)))

;; -> 1 2 4 8 16 ...

s

(stream-ref s 0)
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
(stream-ref s 4)
(stream-ref s 5)

;;;--------------------------< ex 3.54 >--------------------------
;;; p430

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define s (cons-stream 2 (mul-streams s s)))

;; -> 4,16,256,65536
(stream-ref s 0)
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
(stream-ref s 4)
(stream-ref s 5)



(define factorials (cons-stream 1 (mul-streams factorials integers)))

factorials

(stream-ref factorials 0)
(stream-ref factorials 1)
(stream-ref factorials 2)
(stream-ref factorials 3)
(stream-ref factorials 4)
(stream-ref factorials 5)


;;;--------------------------< ex 3.55 >--------------------------
;;; p430

;; 스트림 S를 인자로 받아서
;; S0, S0+S1, S0+S1+S2, ... 을 원소로 갖는 스트림을 내놓는 프로시저

(define (partial-sums stream)
  (define pts
    (cons-stream (stream-car stream)
		 (add-streams (stream-cdr stream)
			     pts)))
  pts)

(define pts (partial-sums integers))


(stream-ref integers 0)
(stream-ref integers 1)
(stream-ref integers 2)
(stream-ref integers 3)
(stream-ref integers 4)
;; 1 2 3 4 5

(stream-ref pts 0)
(stream-ref pts 1)
(stream-ref pts 2)
(stream-ref pts 3)
(stream-ref pts 4)
(stream-ref pts 5)


;;;--------------------------< ex 3.56 >--------------------------
;;; p430

;; 2,3,5 외의 소수 인수를 가지지 않는 양의 정수를 작은 것부터 차례대로 반복하지 않고 늘어놓는 문제

;; 만족하는 스트림 S
;; - S는 1로 시작한다.
;; - (scale-stream S 2)의 원소는 S의 원소다
;; - (scale-stream S 3)과 (scale-stream 5 6)의 원소도 그러하다.
;; - S의 원소는 이게 다다.

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car
			       (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))

;; (define S (cons-stream 1 (merge <??>)))




;;;--------------------------< ex 3.57 >--------------------------
;;; p432



;;;--------------------------< ex 3.58 >--------------------------
;;; p432



;;;--------------------------< ex 3.59 >--------------------------
;;; p432



;;;--------------------------< ex 3.60 >--------------------------
;;; p434



;;;--------------------------< ex 3.61 >--------------------------
;;; p434



;;;--------------------------< ex 3.62 >--------------------------
;;; p435





;;;==========================================
;;; 3.5.3 스트림 패러다임
;;; p435


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 스트림 프로세스로 반복을 표현하는 방법
;;;



;; ex 3.63 ~ 3.65

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 쌍으로 이루어진 무한 스트림
;;; p441




;; ex 3.66 ~ 3.72





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 신호를 표현하는 스트림
;;; p447




;; ex 3.73 ~ 3.76


;;;==========================================
;;; 3.5.4 스트림과 셈미룸 계산법(stream and delayed evaluation)
;;; p451


;; ex 3.77 ~ 3.80

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 정의대로 계산하기(normal-order evaluation)
;;; p458




;;;==========================================
;;; 3.5.5 모듈로 바라본 함수와 물체
;;; p459




;; ex 3.81 ~ 3.82

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 함수형 프로그래밍에서 시간의 문제
;;; p461


