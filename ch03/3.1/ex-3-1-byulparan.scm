
;;; 3. 모듈, 물체, 상태
;;;   3.1 덮어쓰기와 갇힌 상태

;;; 연습문제 3.1

(define (make-accumulator n)
  (lambda (a) (set! n (+ n a))
	  n))

(define A (make-accumulator 5))

(A 10)
(A 10)

;;; 연습문제 3.2

(define (make-monitored f)
  (let ((count 0))
    (define (mf cmd)
      (cond ((eq? cmd 'how-many-calls?) count)
	    ((eq? cmd 'reset-count) (set! count 0))
	    (else
	     (set! count (+ 1 count))
	     (f cmd))))
    mf))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 21)
(s 'how-many-calls?)
(s 'how-many-calls?>)

(s 'reset-count)
(s 100)
(s 'how-many-calls?)



;;; 연습문제 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount) (begin (set! balance (- balance amount))
				   balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch passwd m)
    (if (eq? password passwd) 
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- Make-ACCOUNT" m)))
	(lambda (value) "Incorrect Password")))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'deposit) 100)
((acc 'some-other-password 'deposit) 50)

;;; 연습문제 3.4

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount) (begin (set! balance (- balance amount))
				   balance)
	"Insufficient funds"))
  (define (deposit amount) 
    (set! balance (+ balance amount))
    balance)
  (let ((wrong-call-count 0))
    (define (dispatch passwd m)
      (if (eq? password passwd)
	  (begin
	    (set! wrong-call-count 0)	;연속해서 7번 이상 틀렸을때만 출력되도록....
	    (cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- Make-ACCOUNT" m))))
	  (begin
	    (set! wrong-call-count (+ 1 wrong-call-count))
	    (if (> wrong-call-count 7)
		(lambda (value) "call-the-cops")
		(lambda (value) "Incorrect Password")))))
    dispatch))


(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'deposit) 100)
((acc 'some-other-password 'deposit) 50)



;; 3.1.2 덮어쓰기가 있어서 좋은 점

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
	  ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


;; 아오...Racket 꼬졌어..
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random)))))


;; 연습문제 3.5

;; http://nosyu.pe.kr/photo/15043?page=1128
;; 여기서는 원의 넓이를 적분하여 구하는 문제입니다.
;; 원에 외접하는 사각형을 그린 후 그 사각형 넓이에서 원넓이의 비율이
;; 사각형 안의 아무 곳에 점을 찍었을 때 원 안에 들어가는 확률과 같기에
;; 이를 이용하여 원넓이를 구하는 문제입니다.

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (* (- x2 x1) (- y2 y1)) (monte-carlo trials experiment))) ; 점 개수비를 테두리 넓이와 곱하면 동그란 터의 넓이를 짐작할 수 있다.

(define (P x y)
  (define (square n)
    (* n n))
  (<= (+ (square x) (square y)) 1))	; Why?...이건 터의 중심이 0.0 에 있고 원의 반지름이 1이라고 미리 가정하는 거 아님??

(estimate-integral P -1.0 1.0 -1.0 1.0 100000)
(estimate-integral P 0.0 2.0 0.0 2.0 100000)
(estimate-integral P 2.0 8.0 4.0 10.0 100000)



;; -_-??
(define (square n)
  (* n n))

(define (estimate-integral2 x1 x2 y1 y2 trials)
  (let ((center (cons (* 0.5 (+ x2 x1)) (* 0.5 (+ y2 y1)))))
    (define (P x y)
      (<= (+ (square (- x (car center))) (square (- y (cdr center)))) (square (min (abs (* 0.5 (- x2 x1)))
										   (abs (* 0.5 (- y2 y1)))))))
    (define (experiment)
      (P (random-in-range x1 x2) (random-in-range y1 y2)))
    (* (* (- x2 x1) (- y2 y1)) (monte-carlo trials experiment))))

(estimate-integral2 -1.0 1.0 -1.0 1.0 1000000)
(estimate-integral2 0.0 2.0 0.0 2.0 1000000)
(estimate-integral2 2.0 8.0 4.0 10.0 1000000)


;; 연습문제 3.6
;; Racket...꼬졌어...흥!

(define (rand m)
  (cond ((eq? m 'generate) (random))
	((eq? m 'reset) (lambda (seed) (random-seed seed)))))

((rand 'reset) 65536)
(rand 'generate)
(rand 'generate)
	
((rand 'reset) 65536)
(rand 'generate)
(rand 'generate)

;; 연습문제 3.7

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount) (begin (set! balance (- balance amount))
				   balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((wrong-call-count 0))
    (define (dispatch passwd m)
      (if (eq? password passwd)
	  (begin
	    (set! wrong-call-count 0)	;연속해서 7번 이상 틀렸을때만 출력되도록....
	    (cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- Make-ACCOUNT" m))))
	  (begin
	    (set! wrong-call-count (+ 1 wrong-call-count))
	    (if (> wrong-call-count 7)
		(lambda (value) "call-the-cops")
		(lambda (value) "Incorrect Password")))))
    dispatch))

(define (make-joint account old-password new-password)
  (lambda (password m)
    (if (eq? password new-password)
	(account old-password m)
	(error "Incorrect Password"))))


(define peter-acc (make-account 10 'open-sesame))
((peter-acc 'open-sesame 'deposit) 100)
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 100)


;; 연습문제 3.8

(define f
  (let ((state 0))
    (lambda (n)
      (let ((old state))
	(set! state (+ n state))
	old))))

