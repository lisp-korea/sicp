;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 3 모듈, 물체, 상태
;;; Ch 3.4 병행성: 시간은 중요하다
;;; p385

;; 변수값을 덮어쓸수 있게 되면서 시간 개념을 끌어들일 수 밖에 없다

;; 시간을 끌어들이면 실제로 일어나는 현상에 가까운 프로그래밍을 할 수 있다.


;;;==========================================
;;; 3.4.1 병행 시스템에서 시간의 성질
;;; p387


;; 여러 프로세스가 상태변수 하나를 같이 쓸 때 발생되는 문제.

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 병행 프로그램의 올바른 동작

;; 병행프로그램이 올바로 돌아갈 수 있도록 병행 처리에 어떤 제약을 줄 수 밖에 없다.
;; 제약조건 1)
;; : 같이 쓰는 상태 변수의 값을 바꾸는 연산 두 개가 같은 시간에 돌아가지 않게끔 한다.

;; 약한 제약조건
;; : 병행 시스템이 같은 결과를 만들어 내게끔 보장.
;;  1) 차례대로 돌아가는 것과 결과만 같으면 된다.
;;  2) 올바른 결과는 여러 개일 수 있다.

;;;--------------------------< ex 3.38 >--------------------------
;;; p392

;; 최초에 100원
;; 1) 피터 : 10원 넣음  
;(set! balance (+ balance 10))

;; 2) 폴  : 20원 꺼냄
;(set! balance (- balance 20))

;; 3) 메리 : 절반을 찾아간다.
;(set! balance (- balance (/ balance 2)))

;; a) 세 프로세스가 차례대로 돌아갈 때, 가능한 balance 값 모두
; 1,2,3  : 45
; 1,3,2  : 25
; 2,1,3  : 45
; 2,3,1  : 50
; 3,1,2  : 40
; 3,2,1  : 40

;;-> 25,40,45,50

;; b) 세 프로세스가 뒤섞여 돌아갈 때, 가능한 balance 값
; 1: 100원 확인
; 2: 100원 확인
; 1: 10 넣음   -> 110
; 3: 110원 확인
; 2: 20원 꺼냄 -> 90
; 3: 절반 꺼냄 -> 55

;; 또 다른 방식도 가능...


;;;==========================================
;;; 3.4.2 병행성을 다스리는 방법
;;; p393

;; 줄 세우개

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 한 상태를 여럿이 같이 쓸 때 그 차례를 정하는 방법

; 줄 세우기 : 여러 프로세스가 병행으로 돌아가게 하지만, 그 가운데 병행으로 돌아가지 못하는 프로시저들이 있도록 하는 것.
; (프로시저들을 여러 그룹으로 나누고, 같은 그룹에 속하는 프로시저들이 동시에 실행되는 일이 없도록)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme으로 줄 세우개를 만드는 방법

;; parallel-execute

;; (define x 10)
;; (parallel-execute (lambda () (set! x (* x x)))
;; 		  (lambda () (set! x (+ x 1)))) 

;;; p396
;; (define x 10)

;; (define s (make-serializer))

;; (parallel-execute (s (lambda () (set! x (* x x))))
;; 		  (s (lambda () (set! x (+ x 1)))))
;; 줄세우개 s에 속하는 프로시저들



;;; 3.1.1 의 make-account에서 deposit과 withdraw 에 줄 세우개를 붙임
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
	    ((eq? m 'deposit) (protected deposit))
	    ((eq? m 'balance) balance)
	    (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))


;;;--------------------------< ex 3.39 >--------------------------
;;; p397

(define x 10)

(define s (make-serializer))

;; 
(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
;;                                    ^^^^^^^^^^^^^^^^^^^^^^^:1
;;                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^:2
		  (s (lambda () (set! x (+ x 1)))))
;;                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^:3

;; 1): 1: x를 읽어서 제곱한다. :100
;;     2: 제곱한값을 x에 덮어쓴다. : x:100
;;     3: x를 읽어서 1을 더한 후 x에 덮어쓴다. : x:101

;; 2): 1: x를 읽어서 제곱한다. :100
;;     3: x를 읽어서 1을 더한 후 x에 덮어쓴다. : x:11
;;     2: 제곱한값을 x에 덮어쓴다. : x:100

;; 3): 3: x를 읽어서 1을 더한 후 x에 덮어쓴다. : x:11
;;     1: x를 읽어서 제곱한다. :121
;;     2: 제곱한값을 x에 덮어쓴다. : x:121



;;;--------------------------< ex 3.40 >--------------------------
;;; p397

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
;;                                   ^^^^^^^:1
;;                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^:2
		  (lambda () (set! x (* x x x))))
;;                                   ^^^^^^^^^:3
;;                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^:4

;; 1) 1: 100
;;    2: x:100
;;    3: 1000000
;;    4: x:1000000

;; 2) 1,3,2,4 -> x:1000
;; 3) 1,3,4,2 -> x:100
;; 4) 3,4,1,2 -> x:1000000
;; 5) 3,1,4,2 -> x:100
;; 6) 3,1,2,4 -> x:1000

;; 3가지 - 100, 1000, 1000000

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
;;                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^:1
		  (s (lambda () (set! x (* x x x)))))
;;                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^:2

;; 1) 1,2 -> x:1000000
;; 2) 2,1 -> x:1000000

;; 1가지 - 1000000



;;;--------------------------< ex 3.41 >--------------------------
;;; p397,8

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
	    ((eq? m 'deposit) (protected deposit))
	    ((eq? m 'balance)
	     ((protected (lambda () balance)))) ; serialized
	    (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))


;; balance를 읽기만 하는 경우에는 줄세우개 그룹에 포함시키지 않아도 될 것 같은데??
;; 단 balance의 값을 읽어서 무언가를 하는 프로시저를 짠다면 문제가 될 것이다.



;;;--------------------------< ex 3.42 >--------------------------
;;; p398,9


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))   ;<---
	  (protected-deposit (protected deposit)))    ;<---
      (define (dispatch m)
	(cond ((eq? m 'withdraw) protected-withdraw)
	      ((eq? m 'deposit) protected-deposit)
	      ((eq? m 'balance) balance)
	      (else (error "Unknown request -- MAKE-ACCOUNT" m))))
      dispatch)))

;; 차이 없는 것 같은데,,, 쩝..


