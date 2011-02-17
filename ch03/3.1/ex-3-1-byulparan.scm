
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
