(define (error reason . args)
      (display "Error: ")
      (display reason)
      (for-each (lambda (arg) 
                  (display " ")
		  (write arg))
		args)
      (newline)
      (scheme-report-environment -1))
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



;; 3.5.2 Infinite Streams

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define the-empty-stream '())
(define (stream-null? s)
  (eq? s the-empty-stream))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
		 integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)

;; Defining streams implicitly

(define ones (cons-stream 1 ones))


(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
	(apply proc (map stream-car argstreams))
	(apply stream-map
	       (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
	       (cons-stream 1
			    (add-streams (stream-cdr fibs)
					fibs))))

;; ex 3.53
(define s (cons-stream 1 (add-streams s s)))

;; (1 .
;;      ((+ 1 1)) . (add-stream
;;                              (+ (+ 1 1) (+ 1 1)) (add-stream (+ 1 1)
;;								(+ 1 1)
;;                              (+ (+ 1 1) (+ 1 1)) (add-stream (+ 1 1)
;;								(+ 1 1)
;; ...


;; like [1 2 4 8 16 32 64 ...]

;; ex 3.54

(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams integers factorials)))

;; ex 3.55
(define (sum-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums s)
  (cons-stream 0 (sum-streams s (partial-sums s))))

;; ex 3.56
(define (scale-stream S n)
  (cons-stream (* (stream-car S) n)
	       (scale-stream (stream-cdr S) n)))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car
			       (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream integers 2)
				(merge (scale-stream integers 3)
				       (scale-stream integers 5)))))

;; ex 3.57

;; if (stream-ref fibs 5)

(stream-ref (stream-cdr fibs) 4)
(stream-ref (stream-cdr (stream-cdr fibs)) 3)
(stream-ref (stream-cdr (stream-cdr (stream-cdr fibs))) 2)
(stream-ref (stream-cdr (stream-cdr (stream-cdr (stream-cdr fibs)))) 1)
(stream-ref (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr fibs))))) 0)
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr fibs))))))


;;           1    1    2    3  = '(stream-cdr fibs)' : a
;;              (0+1)(1+1)(2+1)
;;           0    1    1    2  = 'fibs' : b
;;                   (0+1)(1+1)
;; 0    1    1    2    3    5  = 'fibs'
;;         (0+1)(1+1)(2+1)(3+2)
;;         a1+b1
;;              a3+b2=a1+b1+b2
;;                   a4+b3= ...


;; ex 3.58


(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(expand 1 7 10)

;; (1 (expand (remainder 10 7) 7 10))
;; (1 (4 (expand (remainder 30 7) 7 10)))
;; (1 (4 (2 (expand (remainder 20 7) 7 10))))
;; (1 (4 (2 (8 (expand (remainder 60 7) 7 10))))))
;; (1 (4 (2 (8 (5 (expand (remainder 40 7) 7 10))))))
;; (1 (4 (2 (8 (5 (7 (expand (remainder 50 7) 7 10))))))))
;; (1 (4 (2 (8 (5 (7 (1 (expand (remainder 10 7) 7 10))))))))
;; ...


(expand 3 8 10)

;; (3 (expand (remainder 30 8) 8 10))
;; (3 (7 (expand (remainder 60 8) 8 10)))
;; (3 (7 (5 (expand (remainder 40 8) 8 10))))
;; (3 (7 (5 (0 (expand 0 8) 8 10))))
;; ...

;; ex 3.59

;; a

(define (integrate-series s)
  (define (iter s n)
    (cons-stream (/ (stream-car s) n)
		 (iter (stream-cdr s) (+ n 1))))
  (iter s 1))

;; b

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; ex 3.60

;; 45 + 5 + 27 + 3
;; (a1x + a2x^2) * (b1x + b2x^2)
;; (a1x * b1x) + (a1x * b2x^2) + (a2x^2 * b1x) (a2x^2 * b2x^2)
;; (a1b1)x^2 + (a1b2 + a2b1)x^3 + (a2b2)x^4

(define (mul-seriese s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-streams
    (mul-seriese (stream-cdr s1) s2)
    (scale-stream (stream-cdr s2) (stream-car s1)))))

(define expected
  (add-streams
   (mul-seriese sine-series sine-series)
   (mul-seriese cosine-series cosine-series)))

;; ex 3.61

(define (invert-unit-series s)
  (cons-stream
   1
   (scale-stream
    (mul-seriese (stream-cdr s)
		 (invert-unit-series s))
    -1)))

;; ex 3.62

;; sin x = a/h
;; cos x = b/h
;; tan x = a/b = sin x / cos x
(define (div-seriese s1 s2)
  (if (= (stream-car s2) 0)
      (error "stream has 0")
      (mul-seriese s1
		   (invert-unit-series s2))))

(define tan-serise
  (div-seriese sine-series cosine-series))

;; 3.5.3 Exploiting the Stream Paradigm

;; Formulating iterations as stream processes

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)


;; [pi]        1     1     1
;; ---- = 1 - --- + --- - --- + ...
;;   4         3     5     7

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))



;;              (S_(n+1) - S_n)^2
;; S_(n+1) - ------------------------
;;           S_(n-1) - 2S_n + S_(n+1)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

;; ex 3.63

(define (sqrt-stream x)
  (cons-stream 1.0
	       (stream-map (lambda (guess)
			     (sqrt-improve guess x))
			   (sqrt-stream x))))

;; when call (sqrt-stream), create (cons-stream x ...)

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)

;; guesses is cached.


;; ex 3.64

(define (stream-limit s tolerance)
  (let ((s1 (stream-ref s 0))
	(s2 (stream-ref s 1)))
    (if (< (abs (- s1 s2)) tolerance)
	s2
	(stream-limit (stream-cdr s) tolerance))))

(define (sqrt-with-tolerance x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


;; ex 3.65

(define (ln2-summands x)
  (cons-stream (/ 1.0 x)
   (stream-map - (ln2-summands (+ x 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

;; Infinite streams of pairs