;; 4.3 Variations on a Scheme -- Nondeterministic Computing

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
		(b (an-element-of list2)))
	(require (prime? (+ a b)))
	(list a b)))

(define (prime? x)
  (define (iter n)
	(cond ((= n x) #t)
		  ((= (remainder x n) 0) #f)
		  (else
		   (iter (+ 1 n)))))
  (if (< x 2)
	  #f
	  (iter 2)))

;; 4.3.1 Amb and Search


(define-syntax amb
  (syntax-rules ()
    ((amb) (try-again))
    ((amb x) x)
    ((amb x . xs)
     (amb+ (lambda () x)
           (lambda () (amb . xs))))))

(define (try-again)
  (if (null? amb-stack)
      (error "amb search tree exhausted")
      (let ((r (car amb-stack)))
        (set! amb-stack (cdr amb-stack))
        (r))))

(define (amb-reset)
  (set! amb-stack '()))
      
(define amb-stack '())

(define (amb+ a b)
  (define s '())
  (set! s amb-stack)
  (call/cc
   (lambda (r)
     (call/cc
      (lambda (c)
        (set! amb-stack 
              (cons c amb-stack))
        (r (a))))
	 (set! amb-stack s)
     (b))))  

(define call/cc call-with-current-continuation)


(list (amb 1 2 3) (amb 'a 'b))

(try-again)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))


;; Driver loop

;; ;;; Amb-Eval input:
;; (prime-sum-pair '(1 3 5 8) '(20 35 110))
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; (3 20)
;; ;;; Amb-Eval input:
;; try-again
;; ;;; Amb-Eval value:
;; (3 110)
;; ;;; Amb-Eval input:
;; try-again
;; ;;; Amb-Eval value:
;; (8 35)
;; ;;; Amb-Eval input:
;; try-again
;; ;;; There are no more values of
;; (prime-sum-pair (quote (1 3 5 8)) (quote (20 35 110)))
;; ;;; Amb-Eval input:
;; (prime-sum-pair '(19 27 30) '(11 36 58))
;; ;;; Starting a new problem ;;; Amb-Eval value: (30 11)

;; ex 4.35

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
	(let ((j (an-integer-between i high)))
	  (let ((k (an-integer-between j high)))
		(require (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))

(define (an-integer-between low high)
  (require (not (> low high)))
  (amb low (an-integer-between (+ 1 low) high)))

;; ex 4.36

;; replacing an-integer-between by an-integer-starting-from...

(define (a-pythagorean-triple)
  (let ((i (an-integer-starting-from 1)))
	(let ((j (an-integer-starting-from i)))
	  (let ((k (an-integer-starting-from j)))
		(require (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))

;; it goes to infinite loop because (try-again) produces only k.(continuation of k is top of amb-stack)

;; 1^2 + 1^2 vs 1^2
;; 1^2 + 1^2 vs 2^2
;; 1^2 + 1^2 vs 3^2
;; 1^2 + 1^2 vs 4^2
;; 1^2 + 1^2 vs 5^2
;; ...

(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
	(let ((i (an-integer-between 1 k)))
	  (let ((j (an-integer-between 1 i)))
		(require (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))

;; 1^2 + 1^2 vs 1^2
;; 1^2 + 1^2 vs 2^2
;; 2^2 + 1^2 vs 2^2
;; 2^2 + 2^2 vs 2^2
;; 1^2 + 1^2 vs 3^2
;; ...


;; ex 4.37
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
		(hsq (* high high)))
	(let ((j (an-integer-between i high)))
	  (let ((ksq (+ (* i i) (* j j))))
		(require (>= hsq ksq))
		(let ((k (sqrt ksq)))
		  (require (integer? k))
		  (list i j k))))))

;; hsq = high * high
;; ksq = i^2 + j^2
;; filter hsq >= ksq
;; filter sqrt ksq(=k) is integer

;; (a-pythagorean-triple-between 1 10)

;; high = 10
;; low = 1

;; [amb-1]
;; i = 1

;; hsq = 100

;; [amb-2]
;; j = 1
;; ksq = 2

;; require 100 >= 2
;; k = sqrt 2

;; require k is integer => false : to amb-2

;; [amb-2]
;; j == i :to amb-1

;; [amb-1]
;; i = 2

;; hsq = 100

;; [amb-2]
;; j = 1
;; ksq = 5

;; require 100 >= 5
;; k = sqrt 5

;; require k isn't integer => false : to amb-2

;; ...


;; it uses only 2 continuation.(i, j) so time complexity is O(n^2)[4.35's solution is O(n^3)]
