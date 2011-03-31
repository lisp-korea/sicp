;; 3.5 Streams

;; 3.5.1 Streams Are Delayed Lists
(define (Square x)
  (* x x))
(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
	  ((prime? count) (iter (+ count 1) (+ count accum)))
	  (else (iter (+ count 1) accum))))
  (iter a 0))


;; 2.2.3..


(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence)) (cons (car sequence)
					  (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high) '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (sum-primes a b)
  (accumulate +
	      0
	      (filter prime? (enumerate-interval a b))))


(car (cdr (filter prime?
		  (enumerate-interval 10000 1000000))))

;; (stream-car (cons-stream x y)) = x
;; (stream-cdr (cons-stream x y)) = y

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car))
		   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;; cons-stream is a special form
;; (cons-stream <A> <B>)
;; (cons <A> (delay <B>))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; The stream implementation in action

(stream-car
 (stream-cdr
  (stream-filter prime?
		 (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))



;; (stream-car
;;  (stream-cdr
;;   (stream-filter prime?
;; 		 (stream-enumerate-interval 10000 1000000))))

(cons 1000
      (delay (stream-enumerate-interval 10001 1000000)))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))

;; Implementing delay and force

;; (delay <EXP>)
;; (lambda () <EXP>)

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false)
	(result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))

;; (memo-proc (lambda () <EXP>))

;; ex 3.50

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
	(apply proc (map stream-car argstreams))
	(apply stream-map
	       (cons proc (map stream-cdr argstreams))))))

;; ex 3.51

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
;; 0

;; x = [0 . (delay ... )]
(stream-ref x 5)
;; 1
;; 2
;; 3
;; 4

(stream-ref x 7)

;; if (delay) is cached, skip 1~4
;; 1
;; 2
;; 3
;; 4

;; 5
;; 6


;; ex 3.52

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

;; sum = 0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

;; sum = 0 + 1
;; seq = [1 . (delay [1 . (delay ... )])]

(define y (stream-filter even? seq))
;; sum = 1 + 2
;; y = [2 . (delay [3 . (delay [3 . (delay ... )])])]
(define z (stream-filter (labmda (x) (= (remainder x 5) 0)) seq))
;; sum = 3 + 4 + 5
;; y = [2 . (delay [3 . (delay [3 . (delay ... )])])]
;; z = [5 . (delay [5 . (delay [5 . (delay ...)])])]
(stream-ref y 7)
;; sum = 12 + 3 + 4 + 5 + ... 14

(display-stream z)
;; 5
;; 10
;; 15
;; 20

;; sum = 114 + 6 + 7 + ... 20