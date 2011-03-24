(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))


;; ex 3.38

(define balance 100)

;; sequential
(define (serial-withdraw)
  (define (peter)
    (set! balance (+ balance 10)))
  
  (define (paul)
    (set! balance (- balance 20)))
  
  (define (mary)
    (set! balance (- balance (/ balance 2))))
  
  (begin (peter) (paul) (mary) balance))

;; concurrent
(define (crazy-withdraw)
  (define debug? #f)
  (define (peter)
    (display "peter\n")
    (set! balance (+ balance 10)))

  (define (paul)
    (display "paul\n")
    (set! balance (- balance 20)))
  
  (define (mary)
    (display "mary\n")
    (set! balance (- balance (/ balance 2))))
  
  (define list-of-threads (list (thread peter)
                                (thread paul)
                                (thread mary)))

  (begin
    (for-each thread-wait list-of-threads)
    balance))

;; 3.4.2

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda() (set! x (* x x))))
                  (s (lambda() (set! x (+ x 1)))))


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
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; ex 3.39
(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

;; 1: (* x x) --> set x --> set! x (+ x 1) --> 101
;; 2: (* x x) --> set! x (+ x 1) --> set x --> 11

;; ex 3.40
(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;; 1000000 : P_1 sets `x' to 100 and then P_2 sets 'x' to 10000000.
;; 1000000 : P_2 sets `x' to 1000 and then P_1 sets 'x' to 10000000.
;; 10000: P_2 changes `x' from 10 to 1000 between the two
;;      times that P_1 accesses the value of `x' during
;;      the evaluation of `(* x x)'.
;; 100000 : P_2 accesses first `x', then P_1 sets `x' to 100, then P_2 sets `x'.
;; 10000 : P_2 accesses second 'x', then P_1 sets `x' to 100, then P_2 sets `x'.
;; 1000 : P_1 accesses `x' (twice), then P_2 sets `x' to 1000, then P_1 sets `x'.

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

;; 1000000

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

;; withdraw or deposit 전에 balance에 접근?

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

;; not safe because deposit always follows withdraw