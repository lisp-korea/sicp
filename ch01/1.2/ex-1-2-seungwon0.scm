;; ex-1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)

(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9


;; ex-1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)				;1024

(A 2 4)					;65536

(A 3 3)					;65536

(define (f n) (A 0 n))			;2n

(f n)
(A 0 n)
(* 2 n)

(define (g n) (A 1 n)) 			;2^n

(g n)
(A 1 n)
(cond ((= n 1) 2)
      (else (A 0
	       (A 1 (- n 1)))))
(cond((= n 1) 2)
      (else (* 2 (A 1 (- n 1)))))
(cond((= n 1) 2)
      (else (* 2 (g (- n 1)))))
(define (g n)
  (if (= n 1)
      2
      (* 2
	 (g (- n 1)))))

(define (h n) (A 2 n))			;2^(h(n-1)) (h(1) = 2)

(h n)
(A 2 n)
(cond ((= n 1) 2)
      (else (A 1
	       (A 2 (- n 1)))))
(cond ((= n 1) 2)
      (else (A 1
	       (h (- n 1)))))
(define (h n)
  (if (= n 1)
      2
      (g (h (- n 1)))))

(define (k n) (* 5 n n))		;5n^2


;; ex-1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

(define (f n)
  (define f0 0)
  (define f1 1)
  (define f2 2)
  (define (f-iter fk-1 fk-2 fk-3 k n)
    (define fk (+ fk-1 (* 2 fk-2) (* 3 fk-3)))
    (cond ((< n 3) n)
	  ((= k n) fk)
	  (else (f-iter fk fk-1 fk-2 (+ k 1) n))))
  (f-iter f2 f1 f0 3 n))


;; ex-1.12
(define (pascals-triangle i j)
  (cond ((= j 1) 1)
	((= i j) 1)
	(else (+ (pascals-triangle (- i 1)
				   (- j 1))
		 (pascals-triangle (- i 1)
				   j)))))


;; ex-1.13
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (fib2 n)
  (define pi  (/ (+ 1 (sqrt 5)) 2))
  (define phi (/ (- 1 (sqrt 5)) 2))
  (define (fast-expt b n)
    (cond ((= n 0) 1)
	  ((even? n) (square (fast-expt b (/ n 2))))
	  (else (* b (fast-expt b (- n 1))))))
  (/ (- (fast-expt pi n) (fast-expt phi n)) (sqrt 5)))


;; ex-1.14
(cc 11 5)
(cc 11 4) (cc -39 5)
(cc 11 3) (cc -14 4) 0
(cc 11 2) (cc 1 3) 0
(cc 11 1) (cc 6 2) (cc 1 2) (cc -9 3)
(cc 11 0) (cc 10 1) (cc 6 1) (cc 1 2) (cc 1 1) (cc -4 2) 0
0 (cc 10 0) (cc 9 1) (cc 6 0) (cc 5 1) (cc 1 1) (cc -4 2) (cc 1 0) (cc 0 1) 0
0 (cc 9 0) (cc 8 1) 0 (cc 5 0) (cc 4 1) (cc 1 0) (cc 0 1) 0 0 1
0 (cc 8 0) (cc 7 1) 0 (cc 4 0) (cc 3 1) 0 1
0 (cc 7 0) (cc 6 1) 0 (cc 3 0) (cc 2 1)
0 (cc 6 0) (cc 5 1) 0 (cc 2 0) (cc 1 1)
0 (cc 5 0) (cc 4 1) 0 (cc 1 0) (cc 0 1)
0 (cc 4 0) (cc 3 1) 0 1
0 (cc 3 0) (cc 2 1)
0 (cc 2 0) (cc 1 1)
0 (cc 1 0) (cc 0 1)
0 1


;; ex-1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; a
(sine 12.15)
(p (sine (/ 12.15 3.0)))
(p (sine 4.05))
(p (p (sine (/ 4.05 3.0))))
(p (p (sine 1.3499999999999999)))
(p (p (p (sine (/ 1.3499999999999999 3.0)))))
(p (p (p (sine .44999999999999996))))
(p (p (p (p (sine (/ .44999999999999996 3.0))))))
(p (p (p (p (sine .15)))))
(p (p (p (p (p (sine (/ .15 3.0)))))))
(p (p (p (p (p (sine 4.9999999999999996e-2))))))
(p (p (p (p (p 4.9999999999999996e-2)))))
;; 5 times

;; b
(sine a)
;; The order of growth in space: O(log_3(a))
;; The order of growth in number of steps: O(log_3(a))


;; ex-1.16
(define (fast-expt b n)
  (define (fast-expt-iter a b n)
    (cond ((= n 0) a)
	  (else (fast-expt-iter
		 (if (even? n) a (* a b))
		 (if (even? n) (square b) b)
		 (if (even? n) (/ n 2) (- n 1))))))
  (fast-expt-iter 1 b n))


;; ex-1.17
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (halve  x) (/ x 2))
(define (double x) (+ x x))

(define (* a b)
  (cond ((= b 0) 0)
	((even? b) (* (double a) (halve b)))
	(else (+ a (* a (- b 1))))))


;; ex-1.18
(define (* a b)
  (define (iter a b c )
    (cond ((= b 0) c)
	  ((even? b) (iter (double a) (halve b) c))
	  (else (iter a (- b 1) (+ a c)))))
  (iter a b 0))


;; ex-1.19
;; a = b(2pq + q^2) + a(2pq + q^2) + a(p^2 + q^2)
;; b = b(p^ + q^2) + a(2pq + q^2)
;; p' = p^2 + q^2
;; q' = 2pq + q^2
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; compute p'
                   (+ (* 2 p q) (square q))  ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;; ex-1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; normal-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
2

;; applicative-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 50 6)
(gcd 6 (remainder 50 6))
(gcd 6 2)
(gcd 2 (remainder 6 2))
(gcd 2 2)
(gcd 2 (remainder 2 2))
(gcd 2 0)
2


;; ex-1.21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)                  ;199
(smallest-divisor 1999)                 ;1999
(smallest-divisor 19999)                ;7


;; ex-1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes min max)
  (cond ((> min max) #f)
        (else
         (if (even? min)
             #f
             (timed-prime-test min))
         (search-for-primes (+ min 1) max))))

(search-for-primes    1000    1019)
(search-for-primes   10000   10037)
(search-for-primes  100000  100043)
(search-for-primes 1000000 1000037)

(search-for-primes    1000000000    1000000021)
(search-for-primes   10000000000   10000000061)
(search-for-primes  100000000000  100000000057)
(search-for-primes 1000000000000 1000000000063)


;; ex-1.23
(define (next input)
  (if (= input 2)
      3
      (+ input 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(search-for-primes    1000000000    1000000021)
(search-for-primes   10000000000   10000000061)
(search-for-primes  100000000000  100000000057)
(search-for-primes 1000000000000 1000000000063)

;; The observed ration of the speeds of the two algorithms: 1.5 ~ 1.7


;; ex-1.24
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (runtime) start-time))))

(timed-prime-test 1000000007)
(timed-prime-test 1000000009)
(timed-prime-test 1000000021)
(timed-prime-test 10000000019)
(timed-prime-test 10000000033)
(timed-prime-test 10000000061)
(timed-prime-test 100000000003)
(timed-prime-test 100000000019)
(timed-prime-test 100000000057)
(timed-prime-test 1000000000039)
(timed-prime-test 1000000000061)
(timed-prime-test 1000000000063)


;; ex-1.25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))


;; ex-1.26
