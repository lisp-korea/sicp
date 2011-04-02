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

(define (force delayed-object)
  (delayed-object))


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

(define z (stream-filter (lambda (x) 
			   (= (remainder x 5) 0))
			 seq))


