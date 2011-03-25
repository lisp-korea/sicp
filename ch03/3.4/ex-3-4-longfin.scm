;; 3.4 Concurrency: Time is of the Essence

;; 3.4.1 The Nature of Time in Concurrent Systems

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

(set! balance (- balance amount))

;; 1. get balance.
;; 2. compute (- balance amount)
;; 3. set balance.

;; Correct behavior of concurrent programs

;; ex 3.38
;; Peter(P): (set! balance (+ balance 10))
;; Paul(A): (set! balance (- balance 20))
;; Mary(M): (set! balance (- balance (/ balance 2)))

;;a.
;; PAM = (((100 + 10) - 20) / 2) = 45
;; PMA = (((100 + 10) /2) -20) = 35
;; APM = (((100 - 20) + 10) / 2) = 45
;; AMP = (((100 - 20) / 2) + 10) = 50
;; MPA = (((100 / 2) + 10) - 20) = 40
;; MAP = (((100 / 2) - 20) + 10) = 40

;;b. :(

;; 3.4.2 Mechanisms for Controlling Concurrency

;; Serializing access to shared state

;; Serializers in Scheme
(#%require (planet "sicp-concurrency.ss" ("dyoo" "sicp-concurrency.plt" 1 1)))
(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
		  (lambda () (set! x (+ x 1))))

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
		  (s (lambda () (set! x (+ x 1)))))


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
      (cond
       ((eq? m 'withdraw) (protected withdraw))
       ((eq? m 'deposit) (protected deposit))
       ((eq? m' balance) balance)))
    dispatch))

;; ex 3.39

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x)))))) ;; A
		  (s (lambda () (set! x (+ x 1))))) ;; B

;; A-B => 101
;; B-A => 121

;; A = load x - calc (* x x) - set x
;;     =  A1     -     A2         -   A3

;; A1 - B -A2 -A3 => 100
;; A1 - A2 - B - A3 => 100


;; ex 3.40

(define x 10)

(parallel-execute (lambda () (set! x (* x x))) ;;A
		  (lambda () (set! x (* x x x)))) ;;B

;; A - B = 1000000
;; B - A = 1000000

;; A = load x - calc(* x x) - set x
;;   = A1     - A2          - A3

;; B = load x - calc(* x x x) - set x
;;   = B1     - B2            - B3

;; A1 - B1 - A2 - A3 - B2 - B3
;; = A1 - A2 - B1 - A3 - B2 - B3
;; = A1 - B1 -B2 - A2 - A3 - B3
;; = A1 - A2 - B1 - B2 - A3 - B3
;; = A1 ... - B3
;; = 1000(last B3 dominates)

;; B1 - A1 - B2 - B3 - A2 - A3
;; = B1 - B2 - A1 - B3 - A2 - A3 
;; = B1 ... A3
;; = 100 (last A3 dominates)


(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
		  (s (lambda () (set! x (* x x x)))))

;; remain only 1000000

;; ex 3.41

 (define (make-account balance)
   (define (withdraw amount)
     (if (>= balance amount)
	 (begin (set! balance (- balance amount))
		balance)
                  "Insufficient funds"))
   (define (deposit amount)
     (set! balance (+ balance amount))
              balance)
   ;; continued on next page
   
   (let ((protected (make-serializer)))
     (define (dispatch m)
       (cond ((eq? m 'withdraw) (protected withdraw))
	     ((eq? m 'deposit) (protected deposit))
	     ((eq? m 'balance)
	      ((protected (lambda () balance)))) ; serialized
	     (else (error "Unknown request -- MAKE-ACCOUNT"
			  m))))
     dispatch))

;; Ben worries about 'dirty read'. but all operations are atomic on balance(it means balance has no temporary status.)

;; ex 3.42

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
    (let ((protected-withdraw (protected withdraw))
	  (protected-deposit (protected deposit)))
      (define (dispatch m)
	(cond ((eq? m 'withdraw) protected-withdraw)
	      ((eq? m 'deposit) protected-deposit)
	      ((eq? m 'balance) balance)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m))))
      dispatch)))

;; all accounts on global env share same serializer...

;; Complexity of using multiple shared resources

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
		       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    ((eq? m 'balance) balance)
	    ((eq? m 'serializer) balance-serializer)
	    (else (error "Unknown request -- MAKE-ACCOUNT"
			 m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
	(d (account 'deposit)))
    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
	(serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))
;; ex 3.43

;; if processes are run sequentially...

;; A[10], B[20], C[30]
;; th1: (exchange A B)
;; th1: load A:10
;; th1: load B:20
;; th1: calc diff:-10
;; th1: withdraw A:20
;; th1: deposit B:10

;; A[20], B[10], C[30]
;; th2: (exchange B C)
;; th2: load B:10
;; th2: load C:30
;; th2: calc diff:-20
;; th2: withdraw B:30
;; th2: deposi C:10

;; A[20], B[30], C[10]

;; if processes are run parallely

;; A[10], B[20], C[30]
;; th1: (exchange A B)
;; th2: (exchange B C)
;; th1: load A:10
;; th2: load B:20
;; th1: load B:20
;; th2: load C:30
;; th1: calc diff:-10
;; th2: calc diff:-10
;; th1: withdraw A:20
;; th1: deposit B:10
;; th2: withdraw B:20
;; th1: deposit C:20

;; A[20], B[20], C[20]

;; if withdraw, deposit aren't serialized...

;; A[10], B[20], C[30]
;; th1: (exchange A B)
;; th2: (exchange B C)
;; th1: (exchange A B)
;; th2: (exchange B C)
;; th1: load A:10
;; th2: load B:20
;; th1: load B:20
;; th2: load C:30
;; th1: calc diff:-10
;; th2: calc diff:-10
;; th1: withdraw A-load balance:10
;; th1: withdraw A-add balance:20
;; th1: withdraw A-update balance:20
;; th1: deposit B-load balance:20
;; th2: withdraw B-load balance:20
;; th2: withdraw B-add balance:30
;; th2: withdraw B-update balance:30
;; th1: deposit B-add balance:10
;; th1: deposit B-update balance:10
;; th2: deposit C-load balanace:30
;; th2: deposit C-add balance:20
;; th2: deposit C-update balance:20

;; A[20], B[10], C[20]

;; ex 3.44
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;;transfer has no temporary status (=diff). don't need mutex.

;; ex 3.45
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
                      ((eq? m 'deposit) (balance-serializer deposit))
                      ((eq? m 'balance) balance)
                      ((eq? m 'serializer) balance-serializer)
                      (else (error "Unknown request -- MAKE-ACCOUNT"
                                   m))))
    dispatch))

(define (deposit account amount)
  ((account 'deposit) amount))

;; balance-serializer is just account local, not inter-account serializer.
