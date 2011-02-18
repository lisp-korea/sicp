;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 3 모듈, 물체, 상태
;;; Ch 3.1 덮어쓰기와 갖힌 상태

;; 물체마다 시간에 따라 변하는 상태가 있다고 보는 관점.
;; 물체의 상태 - 상태변수


;; 진짜 물체에 대응되는 계산 물체를 만든다.
;; 계산 물체 속에 상태변수를 감춰 놓고,
;; 진짜 물체의 상태가 바뀌는 것을, 상태변수가 바뀌는 것으로 흉내낸다.
;;-> 
;; 변수의 값을 다른 값으로 덮어쓰는 연산이 필요하다.


;;;==========================================
;;; 3.1.1 갇힌 상태변수

;; p288

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

;;
(set! balance (- balance 10))


;; p290
;; withdraw에 갖힌 balance 만들기
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))
;;-> let over lambda
;;->  lambda 위에 let
;;-> closure
;; let 으로 새 환경을 만들고,
;; 그 속에서 변수 balance를 정의
;; 같은 환경 속에서 amount인자를 받는 프로시저

;; p291
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50)
(W2 70)
(W2 40)
(W1 40)

;; p292
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  dispatch)
;; make-account 를 돌릴 때마다 상태변수 balance가 들어있는 새 환경이 마련된다.
;; '메시지 패싱'

(define acc (make-account 100))

((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)

(define acc2 (make-account 100))

;;;--------------------------< ex 3.1 >--------------------------
;;; p293
;;; 어큐뮬레이터 : 여러 수를 차례대로 이어 받아서 합을 내는 프로시저
(define (make-accumulator n)
  (define (accumulator x)
    (set! n (+ n x))
    n)
  accumulator)

(define A (make-accumulator 5))

(A 10)

(A 10)

;;;--------------------------< ex 3.2 >--------------------------
;;; p294
;; 프로시저가 불린 횟수를 알 수 있도록 하는 물체
(define (make-monitored f)
  (define call-count 0)
  (define (mf x)
    (if (eq? x 'how-many-calls?)
	call-count
	(begin (set! call-count (+ call-count 1))
	       (f x))))
  mf)
    
(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)

;;;--------------------------< ex 3.3 >--------------------------
;;; p294
;; 암호가 걸린 계정
(define (make-account balance st-password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (if (eq? password st-password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m)))
	(error "Incorrect password")))
  dispatch)


(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)


;;;--------------------------< ex 3.4 >--------------------------
;;; p294
;; 암호가 걸린 계정 + 암호 틀린 횟수 세는 물체
(define (make-account balance st-password)
  (define cnt-miss 0)
  (define cnt-call-cops 7)
  ;; (define (eat-arg x)
  ;;   cnt-miss)
  (define (call-the-cops)
    (error "Call the cops!"))
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (if (eq? password st-password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m)))
	(begin (set! cnt-miss (+ cnt-miss 1))
	       (if (> cnt-miss cnt-call-cops)
		   (call-the-cops)
		   0)
;;	       cnt-miss)))
	       ;;eat-arg)))
	       (error "Incorrect password"))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)

;;;==========================================
;;; 3.1.2 덮어쓰기가 있어서 좋은 점


;; p296
;;---
;; 내 마음대로 만든 random-init, rand-update
(define random-init 1)

;; ;; 1)
;; (define (rand-update x)
;;   (remainder (current-milliseconds) 10))

;; (define (average-rand n)
;;   (define (iter acc c)
;;     (if (= c 0)
;; 	(/ acc n)
;; 	(iter (+ acc (rand))
;; 	      (- c 1))))
;;   (iter (rand) (- n 1)))

;; (* 1.0 (average-rand 100000))

;; 2)
(define (rand-update x)
  (let ((a 37)
  	(b 19)
  	(m 101))
    (remainder (+ (* a x) b) m)))
  ;; (let ((a 51)
  ;; 	(b 71)
  ;; 	(m 103))
  ;;   (remainder (+ (* a x) b) m)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(rand)
;; 56, 71, 20, 52, 24, 99, 46, ...
;;---

;;;------------------------------------------------------
;;; 몬테 카를로 시뮬레이션(Monte Carlo simulation)
      
;;---
;; p63
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;;---

;;;-------------------------------------------
;; p297
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(estimate-pi 100000)

;;;--------------------------------------------
;; rand를 쓰지 않고, rand-update만 써서 같은 프로그램 짜기
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
	(cond ((= trials-remaining 0)
	       (/ trials-passed trials))
	      ((= (gcd x1 x2) 1)
	       (iter (- trials-remaining 1)
		     (+ trials-passed 1)
		     x2))
	      (else
	       (iter (- trials-remaining 1)
		     trials-passed
		     x2))))))
  (iter trials 0 initial-x))

(estimate-pi 100000)

;;;--------------------------< ex 3.5 >--------------------------
;;; p299
;; 몬테 카를로 적분

;;     +--+--+ (8,10)
;;     |(   )|
;;     +--+--+
;;(2,4) 

;; ;; 책에 있는 구현
;; ;; racket에서 정수만 대응되는 문제가 있음.
;; (define (random-in-range low high)
;;   (let ((range (- high low)))
;;     (+ low (random range))))

;; 별파란님 구현
;; : 이 구현이 더 좋다.
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random)))))

;;--
(define (square x) (* x x))

(define (radius x1 x2)
  (/ (- x2 x1) 2))
;;(- x2 x1))

(define (center x1 x2 y1 y2)
  (let ((cx (/ (+ x2 x1) 2))
	(cy (/ (+ y2 y1) 2)))
    (cons cx cy)))

(define (area-rect x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

;; (x,y) 좌표가 (cx, cy)를 중심으로 하고 반지름이 r인 원 안에 있는지 확인.
(define (P x y)
  (lambda (r c)
    (let ((cx (car c))
	  (cy (cdr c)))
      (<= (+ (square (- x cx))
	     (square (- y cy)))
	  (square r)))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define r (radius x1 x2))
  (define c (center x1 x2 y1 y2))
  (define (in-circle?)
    (let ((x (random-in-range x1 x2))
	  (y (random-in-range y1 y2)))
      ((P x y) r c)))
  (let ((area (area-rect x1 x2 y1 y2)))
    (* area (monte-carlo trials in-circle?))))

;;
(* 1.0 (estimate-integral P 2 8 4 10 100000))
;; 반지름 : 3
;; -> 원의 넓이 : 3.14 * 3^2 = 28.26
;; 비슷하게 나옴. 기존에 좌표 오류. 그래도 정수만 다루므로 오차가 크다.
;; randdom-in-range를 별파란님 구현으로하면 더 정확하게 나온다.

;; 더 큰 원에 대해서
(* 1.0 (estimate-integral P 1 101 1 101 100000))
;; 반지름 : 50
;; -> 원의 넓이 : pi*r^2 = 3.14 * 50^2 = 7850
;; 거의 비슷하게 나옴. 

(* 1.0 (estimate-integral P -10 +10 -10 +10 100000))
;; 반지름 : 10
;; -> 원의 넓이 : 3.14 * 10^2 = 314
;; 거의 비슷하게 나옴.

;;;--------------------------< ex 3.6 >--------------------------
;;; p300

(define random-init 1)

(define (rand-update x)
  (let ((a 37)
	(b 19)
	(m 101))
    (remainder (+ (* a x) b) m)))

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) 
	     (begin
	       (set! x (rand-update x))
	       x))
	    ((eq? m 'reset)
	     (lambda (v)
	       (set! x v)))))))

(rand 'generate)
;; 56, 71, 20, 52, 24, 99, 46, 4, 66, 37, ...

((rand 'reset) 1)

(rand 'generate)

((rand 'reset) 10)

(rand 'generate)
;; 86 70 84, 97, 73, 94, 63, ...



;;;==========================================
;;; 3.1.3 덮어쓰기를 끌어들인 대가

;; 맞바꿈 계산법을 쓰지 못하는 문제.

;;-----------------
;; set! 을 쓰는 경우
;; p301
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))

(W 20)
; 5

(W 10)
; -5


;;-----------------
;; set! 을 쓰지 않는 경우
(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))

(D 20)
; 5

(D 10)
; 15

;;; 맞바꿈 계산 방식으로 설명가능
((make-decrementer 25) 20) ; 5
;;->
((lambda (amount) (- 25 amount)) 20) ; 5

;;; 맞바꿈 계산 방식으로 설명불가
((make-simplified-withdraw 25) 20)
;;->
;((lambda (amount) (set! balance (- 25 amount)) 25) 20)
;;                      ^^^^^^^    ^^          ^^
;;                      l-value    r-value     r-value set! 이후
;;                                 set! 이전
;;
;;: set! 이전과 이후의 balance가 다르다고 봐야한다.
;;  - 맞바꿈 계산법에서 변수는 그저 값에 붙인 이름
;;  - set!으로 변수 값을 덮어쓸 수 있게 되면서, 변수는 그저 값의 이름이라 할 수는 없다.

;;;-------------------------------
;;; 같음과 달라짐(변함)
;;; p303

(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))
;; D1과 D2는 같다고 할 수 있다.

(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))
;; W1과 W2는 같다고 할 수 없다.

;;; (말)뜻이 한결같은 언어 (referential transparent language)
;;; : 같은 것을 같은 것으로 맞바꿀 수 있다.
;;; set!이 들어가면서 숨은 뜻이 겉으로 드러나지 않아
;;; 같아 보이는 식이 맞바꿔도 되는 식인지 알기 어려워진다.
;;; -> 식에 담긴 뜻이 훤히 드러나지 않으면, 두 계산 물체가 '같다'는 개념을 수학으로 정리하여 설명하기 어렵다.


;;;-------------------------------
;;; 명령을 내려서 프로그램 짤 때 생기는 문제
;;; p306

;;; 명령중심프로그래밍(imperative programming)
;;; : 닾어쓰기처럼 명령을 내려서 프로그램을 짜는 방식
;;-> 계산하는 방법이 복잡해질뿐더러 함수로 짤 때에는 나타나지 않는 오류가 생기기 쉽다.

;; 함수형
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product)
	      (+ counter 1))))
  (iter 1 1))

(factorial 10) ; 3628800

;; 명령형
(define (factorial n)
  (let ((product 1)
	(counter 1))
    (define (iter)
      (if (> counter n)
	  product
	  (begin (set! product (* counter product))
		 (set! counter (+ counter 1))
		 (iter))))
    (iter)))

(factorial 10) ; 3628800

;; 잇달아 덮어쓰기를 할 때에는 덮어쓰는, 차례가 뒤바뀌지 않게 무척 조심해야 한다.


;;;--------------------------< ex 3.7 >--------------------------
;;; p308

(define (make-account balance st-password)
  (define password-list '())
  (define (ok-password? pswd-list passwd)
    (cond ((null? pswd-list) #f)
	  ((eq? (car pswd-list) passwd) #t)
	  (else
	   (ok-password? (cdr pswd-list) passwd))))
  (define (add-password passwd)
    (set! password-list (cons passwd password-list)))
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (make-joint passwd2)
    (add-password passwd2)
    dispatch)
  (define (dispatch password m)
    (if (ok-password? password-list password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      ((eq? m 'make-joint) make-joint)
	      ((eq? m 'list-password) password-list)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m)))
	(error "Incorrect password")))
  (add-password st-password)
  dispatch)

(define (make-joint acc passwd1 passwd2)
  ((acc passwd1 'make-joint) passwd2))

;;===
;; TEST ->
(define peter-acc (make-account 100 'open-sesame))

((peter-acc 'open-sesame 'withdraw) 10)

((peter-acc 'open-sesame 'deposit) 10)

;;===
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 10)

((peter-acc 'open-sesame 'deposit) 10)

((peter-acc 'aaa 'withdraw) 10)


((paul-acc 'rosebud 'withdraw) 5)

((paul-acc 'rosebud 'deposit) 5)

((paul-acc 'aaa 'withdraw) 5)


((peter-acc 'open-sesame 'withdraw) 10)

((peter-acc 'open-sesame 'deposit) 10)

((peter-acc 'aaa 'withdraw) 10)


((peter-acc 'rosebud 'withdraw) 10)

((peter-acc 'rosebud 'deposit) 10)

((peter-acc 'aaa 'withdraw) 10)


((paul-acc 'rosebud 'withdraw) 5)

((paul-acc 'rosebud 'deposit) 5)

((paul-acc 'aaa 'withdraw) 5)


;;;--------------------------< ex 3.8 >--------------------------
;;; p309

(define (make-f x)
  (lambda (a)
    (set! x (* x a))
    x))
;; Let Over Lambda

;; 아래도 같은 것이 됨.
;; (define (make-f b)
;;   (let ((x b))
;;     (lambda (a)
;;       (set! x (* x a))
;;       x)))
;; Let Over Lambda

;;---
(define f (make-f 1))

(+ (f 0) (f 1))
;; 0


;;---
(define f (make-f 1))

(+ (f 1) (f 0))
;; 1
