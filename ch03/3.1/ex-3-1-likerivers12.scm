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
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;;;------------------------------------------------------
;;; 몬테 카를로 시뮬레이션(Monte Carlo simulation)

;;---
;; 내 마음대로 만든 rand, random-init, rand-update
(define (rand)
  (remainder (current-milliseconds) 1000))

(define (random-init)
  (remainder (current-milliseconds) 1000))

(define (rand-update x)
  (remainder (current-milliseconds) 1000))
;;---

	      
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

(estimate-pi 10000)

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

(estimate-pi 10000)

