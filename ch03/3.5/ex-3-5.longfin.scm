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

;; (stream-filter (lambda (pair)
;;                       (prime? (+ (car pair) (cadr pair))))
;;                     int-pairs)

;; int-paires
;; S = (S_i), T = (T_j)

;; (S_0, T_0)  (S_0, T_1)  (S_0, T_2)  ...
;; (S_1, T_0)  (S_1, T_1)  (S_1, T_2)  ...
;; (S_2, T_0)  (S_2, T_1)  (S_2, T_2)  ...
;; ...


;; (S_0, T_0)  (S_0, T_1)  (S_0, T_2)  ...
;;             (S_1, T_1)  (S_1, T_2)  ...
;;                         (S_2, T_2)  ...
;;                                     ...

(pairs integers integers)

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (<COMBINE-IN-SOME-WAY>
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

 (define (stream-append s1 s2)
       (if (stream-null? s1)
           s2
           (cons-stream (stream-car s1)
                        (stream-append (stream-cdr s1) s2))))

;; unsuitable for infinite stream.(s2 isn't appeared.)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; ex 3.66

(define x (pairs integers integers))

;; (pairs [1 ?] [1 ?])
;; [(1 1) (interleave (stream-map (lambda (x) (list 1 x)) [2 ?]) (pairs [2 ?] [2 ?]))]

(stream-ref x 0)
;; (1 1)

(stream-ref x 1)
;; (1 2)

;; (pairs [2 ?] [2 ?])
;; [(2 2) (interleave (stream-map (lambda (x) (list 2 x)) [3 ?]) (pairs [3 ?] [3 ?]))]

(stream-ref x 2)
;; (2 2)

(stream-ref x 3)
;; (1 3)

(stream-ref x 4)
;; (2 3)

(stream-ref x 5)
;; (1 4)


;; (pairs [3 ?] [3 ?])
;; [(3 3) (interleave (stream-map (lambda (x) (list 3 x)) [4 ?]) (pairs [4 ?] [4 ?]))]

(stream-ref x 6)

;; (3 3)


;; (s_1 t_1)
;; (s_1 t_2)
;;	(s_2 t_2)
;; (s_1 t_3)
;;	(s_2 t_3)
;; (s_1 t_4)
;;		(s_3 t_3)
;; (s_1 t_5)
;;      (s_2 t_4)
;; (s_1 t_6)
;;		(s_3 t_4)
;; (s_1 t_7)
;;      (s_2 t_5)
;; (s_1 t_8)
;;                   (s_4 t_4)

;; (1 y) => if (y == 1) : 1, (y != 1) (y-1)*2
;; (1 100) => 99 * 2 

;; (2 y) => if (y == 2) : 3, (y != 2) (y-2)*4
;; (2 100) => 98 * 4

;; (3 y) => if (y == 3) : 7, (y != 3) (y-3)*8+2
;; (3 100) => 97 * 8 +2

;; (4 y) => if (y == 5) : 7, (y != 3) (y-4)*16+6
;; (4 100) => 96 * 16 +6

;; (x y) => if (x == y) : 2^x - 1, (x != y) (y-x)*2^x+2(2^(x-2)-1)
;; (99 100) => (100-99)*2^99+2(2^97-1) = 2^99+2^98-2
;; (100 100) => 2^100 - 1

;;  ex 3.67

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define x (pairs integers integers))

;; ex 3.68


(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (pairs (stream-cdr s) (stream-cdr t))))

(define x (pairs integers integers))

;; hanging...
;; interleave isn't special form. so pairs is eager evaluation.


;; ex 3.69

(define (triples s t u)
  (cons-stream
   (list (stream-car s)
	 (stream-car t)
	 (stream-car u)) 
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
		(pairs (stream-cdr t)
		       (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define ts (triples integers integers integers))
(define pythagorean-triples
  (stream-filter (lambda (t)
		   (= (+ (square (car t))
			 (square (cadr t)))
		      (square (caddr t))))
		 ts))

;; ex 3.70

(define (merge-weighted s1 s2 w)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((w s1car s2car)
		  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 w)))
		 (else
		  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) w))))))))


(define (pairs-weighted s t w)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs-weighted (stream-cdr s) (stream-cdr t) w)
    w)))


(define as (pairs-weighted
	    integers
	    integers
	    (lambda (x y)
	      (let ((x1 (car x))
		    (x2 (cadr x))
		    (y1 (car y))
		    (y2 (cadr y)))
		(< (+ x1 x2) (+ y1 y2))))))

(define S (cons-stream 1 (merge (scale-stream integers 2)
				(merge (scale-stream integers 3)
				       (scale-stream integers 5)))))
(define bs (pairs-weighted
	    S
	    S
	    (lambda (x y)
	      (let ((x1 (car x))
		    (x2 (cadr x))
		    (y1 (car y))
		    (y2 (cadr y)))
		(< (+ (* 2 x1)
		      (* 3 x2)
		      (* 5 x1 x2))
		   (+ (* 2 y1)
		      (* 3 y2)
		      (* 5 y1 y2)))))))

;; ex 3.71

(define (cube x) (* x x x))

(define (ramanujan-filter s)
  (let ((x (stream-car s))
	(y (stream-car (stream-cdr s))))
    (cond ((stream-null? s) the-empty-stream)
	  ((= x y)
	   (cons-stream (stream-car s)
			(ramanujan-filter (stream-cdr s))))
	  (else
	   (ramanujan-filter (stream-cdr s))))))
	   
(define ramanujan-stream
  (ramanujan-filter
   (stream-map
    (lambda (pair)
      (+ (cube (car pair))
	 (cube (cadr pair))))
    (pairs-weighted
     integers
     integers
     (lambda (x y) 
       (let ((x1 (car x))
	     (x2 (cadr x))
	     (y1 (car y))
	     (y2 (cadr y)))
	 (< (+ (cube x1) (cube x2))
	    (+ (cube y1) (cube y2)))))))))

;; > (stream-ref ramanujan-stream 0)
;; 1729
;; > (stream-ref ramanujan-stream 1)
;; 4104
;; > (stream-ref ramanujan-stream 2)
;; 13832
;; > (stream-ref ramanujan-stream 3)
;; 20683
;; > (stream-ref ramanujan-stream 4)
;; 32832
;; > (stream-ref ramanujan-stream 5)
;; 39312

(define (three-way-filter s)
  (let ((s1 (stream-car s))
	(s2 (stream-car (stream-cdr s)))
	(s3 (stream-car (stream-cdr (stream-cdr s)))))
    (let ((s1-sum (+ (square (car s1)) (square (cadr s1))))
	  (s2-sum (+ (square (car s2)) (square (cadr s2))))
	  (s3-sum (+ (square (car s3)) (square (cadr s3)))))
      (cond ((stream-null? s) the-empty-stream)
	    ((= s1-sum s2-sum s3-sum)
	     (cons-stream (list s1-sum s1 s2 s3)
			  (three-way-filter (stream-cdr s))))
	    (else (three-way-filter (stream-cdr s)))))))

(define three-square-ns
  (three-way-filter
   (pairs-weighted
    integers
    integers
    (lambda (x y)
      (let ((x1 (car x))
	    (x2 (cadr x))
	    (y1 (car y))
	    (y2 (cadr y)))
	(< (+ (square x1) (square x2))
	   (+ (square y1) (square y2))))))))
	     	    
;; > (stream-ref three-square-ns 0)
;; (325 (10 15) (6 17) (1 18))
;; > (stream-ref three-square-ns 1)
;; (425 (13 16) (8 19) (5 20))
;; > (stream-ref three-square-ns 2)
;; (650 (17 19) (11 23) (5 25))


;; Streams as signals

;;            i
;;           ---
;; S_i = C + >   x_j dt
;;           ---
;;           j=1

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)

;; *Figure 3.32:* The `integral' procedure viewed as a
;; signal-processing system.

;;                                   initial-value
;;                                        |
;;             +-----------+              |   |\__
;;      input  |           |      |\__    +-->|   \_  integral
;;      ------>| scale: dt +----->|   \_      |cons_>--*------->
;;             |           |      | add_>---->| __/    |
;;             +-----------+  +-->| __/       |/       |
;;                            |   |/                   |
;;                            |                        |
;;                            +------------------------+


;; ex 3.73

;; *Figure 3.33:* An RC circuit and the associated signal-flow
;; diagram.

;;        +                 -
;;       ->----'\/\/\,---| |---
;;        i                 C

;;                    / t
;;                    |  i
;;       v  =  v   +  |      dt + R i
;;              0     |
;;                    / 0

;;               +--------------+
;;           +-->|   scale: R   |---------------------+   |\_
;;           |   +--------------+                     |   |  \_
;;           |                                        +-->|    \   v
;;        i  |   +--------------+     +------------+      | add >--->
;;       ----+-->|  scale: 1/C  |---->|  integral  |----->|   _/
;;               +--------------+     +------------+      | _/
;;                                                        |/

(define (RC r c dt)
  (lambda (i v_0)
    (add-streams
     (scale-stream i r)
     (integral (scale-stream i (/ 1 c)) v_0 dt))))

;; ex 3.74

;; ... 1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4 ...
;; ... 0  0   0   0    0   -1    0   0   0    0    1   0  0 ...

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
			(stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings
  (stream-map sign-change-detector sense-data
	      (cons-stream 0 sense-data)))

;; ex 3.75

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
		 (make-zero-crossings (stream-cdr input-stream)
				      (stream-car input-stream)
				      avpt))))

;; ex 3.76

(define (smooth s)
  (let ((s1 (stream-car s))
	(s2 (stream-car (stream-cdr s))))
  (cons-stream
   (/ 2 (+ s1 s2))
   (smooth s))))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
			(stream-car input-stream))))

(define zero-crossings (make-zero-crossings (smooth sense-data) 0))
  
