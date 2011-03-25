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

;; [peter, paul, mary] -> 45
;; [peter, mary, paul] -> 35
;; [mary, peter, paul] -> 40
;; [mary, paul, peter] -> 40
;; [paul, peter, mary] -> 45
;; [paul, mary, peter] -> 50


;; PLT Scheme implementation
(define (parallel-execute . thunks)
  (for-each thread thunks))

(define (make-serializer)
  (let ((mutex (make-semaphore 1)))
    (lambda (p)
      (define (serialized-p . args)
        (semaphore-wait mutex)
        (let ((val (apply p args)))
          (semaphore-post mutex)
          val))
      serialized-p)))

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

;; 여러자원을 함께 쓰는 문제

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

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

;; refer to longfin's solution


;; ex 3.44
(define (transfer from-account to-account account)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

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

;; not works in inter-account

;; 줄 세우개 만들기
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))


;; 3.46

;; test와 set 중간에 값이 바뀔수 있다.

;; 3.47

;; a.
(define (make-semaphore-mtx maximal)
  (let ((count maximal)
        (mutex (make-mutex)))
    (define (the-sema m)
      (cond ((eq? m 'release)
              (mutex 'acquire)
              (unless (= count maximal)
                (set! count (+ 1 count)))
              (mutex 'release))
            ((eq? m 'acquire)
              (mutex 'acquire)
              (cond
                ((> count 0)
                  (set! count (- count 1))
                  (mutex 'release))
                (else
                  (mutex 'release)
                  (the-sema 'acquire))))
            (else
              (error "Unknown request -- " m))))
    the-sema))

;; b.
(define (loop-test-and-set! cell)
  (if (test-and-set! cell)
    (loop-test-and-set! cell)
    '()))

(define (make-semaphore-ts maximal)
  (let ((count maximal)
        (guard (cons #f '())))
    (define (the-sema m)
      (cond ((eq? m 'release)
              (loop-test-and-set! guard)
                (unless (= count maximal)
                  (set! count (+ 1 count)))
              (clear! guard))
            ((eq? m 'acquire)
              (cond
                (loop-test-and-set! guard)
                ((> count 0)
                  (set! count (- count 1))
                  (clear! guard))
                (else
                  (clear! guard)
                  (the-sema 'acquire))))
            (else
              (error "Unknown request -- " m))))
    the-sema))

;; deadlock

;; ex 3.48
(define (make-account number balance)
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
            ((eq? m 'number) number)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'number) (account2 'number))
      ((serializer2 (serializer1 exchange))
        account1 account2)
      ((serializer1 (serializer2 exchange))
        account1 account2))))