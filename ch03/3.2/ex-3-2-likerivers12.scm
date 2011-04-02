;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 3 모듈, 물체, 상태
;;; Ch 3.2 환경 계산법

;; 환경 : 변수 일람표를 한 줄로 이어 놓은 것.
;; 변수 일람표 : 변수값을 정의하여 모아둔 표
;;  - 꼬리를 따라 그 표를 둘러싸는 환경으로 나간다.
;;  - 맨바깥쪽 일람표에는 꼬리가 없다.


;;;==========================================
;;; 3.2.1 계산 규칙

;;; 환경 계산법
;;; : 프로시저는 코드와 환경을 가리키는 꼬리를 쌍으로 묶어서 나타낸다.


;; define 식을 계산하여 얻은 프로시저 물체는 코드와 환경 꼬리를 묶은 쌍이다.

;; 프로시저 정의
;; p313

;; 프로시저 적용
;; p314


;;;==========================================
;;; 3.2.2 간단한 프로시저 계산하기


;;;--------------------------< ex 3.9 >--------------------------
;;; p317
;; 어떤 얼개로 환경이 만들어지는지 나타내보라

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))



;;;==========================================
;;; 3.2.3 물체에 상태를 넣어두는 곳, 변수 일람표

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

(define W1 (make-withdraw 100))



;;;--------------------------< ex 3.10 >--------------------------
;;; p323
;; 두 make-withdraw가 만들어낸 물체가 똑같이 움직인다는 사실을 밝혀라 -_-

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

(define W1 (make-withdraw 100))

(W1 50)

(define W2 (make-withdraw 100))

(W2 20)


;;;==========================================
;;; 3.2.4 안쪽 정의

;; 프로시저 안에서 이름을 정의하는 기법
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; (define (good-enough? guess)
;;   (< (abs (- (square guess) x)) 0.001))

(define (square x) 
  (* x x))

;; sqrt는 맨 바깥쪽 환경에 프로시저 물체로 정의
;;  - sqrt를 부르면 새 환경 E1이 생긴다.

;; good-enough?, improve, sqrt-iter 는 E1에서 정의된다.
;;  - 코드 -> 각자 자기 몸통을 가리킨다.
;;  - 꼬리 -> E1을 가리킨다.


;; 갇힌 프로시저 정의
;;  1) 안쪽 이름과 바깥쪽 이름이 뒤섞일 염려가 없다.
;;  2) 안쪽에 가두어 정의한 프로시저는 그것을 둘러싼 프로시저의 인자를 자유변수처럼 쓸 수 있다.
;;     - 클로저(closure)


;;;--------------------------< ex 3.11 >--------------------------
;;; p326

;; 1) 상태있는 프로시저
;; 2) 안쪽 정의

;; 메시지 패싱은 보통 위 두 기법을 이용해서 모두 쓴다.

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

(define acc (make-account 50))

((acc 'deposit) 40)

((acc 'withdraw) 60)

(define acc2 (make-account 100))

;; 두 계정의 상태가 어떻게 해서 따로 관리되는가?
;; 환경의 얼개를 살펴볼 때 acc와 acc2가 함께 쓰는 부분은 어디인가?

