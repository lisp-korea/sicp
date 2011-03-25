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

