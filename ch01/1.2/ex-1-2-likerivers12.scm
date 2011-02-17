;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ch1.2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p42
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p43-44
(define (factorial n)
  (fact-iter 1 1 n))

(define fact-iter (product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p46
;;; ex 1.9
(define + (a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define + (a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p47
;;; ex 1.10
;;; ackerman function
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p48
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p50
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p52-53
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(count-change 100)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p54
;;; ex 1.11
;;; 1) if n < 3, f(n) = n
;;; 2) if n >= 3,  f(n) = (n-1)+ 2f(n-2) + 3f(n-3)

;; recursive process
(define (f n)
  (cond ((< n 3) n)
	(else (+ (- n 1)
		 (* 2 (f (- n 2)))
		 (* 3 (f (- n 3)))))))

;; iterative process
(define (f-i n)
  (define (f-iter fn-1 fn-2 fn-3 count)
    (define res-cur (+ (- count 1) (* 2 fn-2) (* 3 fn-3)))
    (cond ((= count n) res-cur)
	  (else (f-iter res-cur
			fn-1 
			fn-2 
			(+ count 1)))))
  (cond ((< n 3) n)
	(else (f-iter 2 1 0 3))))

(f 4)
(f-i 4)
(f 5)
(f-i 5)
(f 6)
(f-i 6)
(f 7)
(f-i 7)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p54
;;; ex 1.12
;;; Pascal's triangle
;;;              1
;;;            1   1
;;;          1   2   1
;;;        1   3   3   1
;;;      1   4   6   4   1
;;;             ...

(define (next-tri lst)
  (cond ((null? lst) '())
	((null? (cdr lst)) '())
	(else (cons (+ (car lst) (cadr lst)) 
		    (next-tri (cdr lst))))))
;;(next-tri '(1 2 1))
;;(next-tri '(1 3 3 1))

(define (pascal-tri n)
  (cond ((= n 1) '(1))
	((= n 2) '(1 1))
	(else (append (cons 1 (next-tri (pascal-tri (- n 1))))
		      '(1)))))

(pascal-tri 1)
(pascal-tri 2)
(pascal-tri 3)
(pascal-tri 4)
(pascal-tri 5)
(pascal-tri 6)
(pascal-tri 7)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p54
;;; ex 1.13
;;; a proof-problem - skip






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p56
;;; ex 1.14
;;...
;;               (cc 100 5)
;;                /       \
;;      (cc 100 4)         (cc 50 5)
;;          / \                  / \
;; (cc 100 3) (cc 75 4)  (cc 50 4) (cc 0 5)
;; ...






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p56
;;; ex 1.15
;;; sin x = 3sin(x/3) - 4(sin(x/3))^3
(define (abs x)
  (if (> x 0)
      x
      (- x)))

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p57

;; recursive process
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(expt 3 4)

;; iterative process
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* b product))))

(expt 3 4)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p58
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(expt 3 4)
(fast-expt 3 4)
(expt 4 2)
(fast-expt 4 2)
(expt 9 11)
(fast-expt 9 11)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p59
;;; ex 1.16
;;; iterative fast-expt

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))



(define (fast-expt-iter base acc acc-tmp count n)
  (cond ((= n 1) (* acc base))
	((= count n) acc)
	((= (* count 2) n) (* acc (square acc-tmp)))
	((< (* count 2) n) (fast-expt-iter base
					   acc
					   (square acc-tmp)
					   (* count 2)
					   n))
	(else (fast-expt-iter base
			      (* acc acc-tmp)
			      base 
			      1
			      (- n count)))))
 
(define (fast-expt-i base n)
  (cond ((= n 0) 1)
	(else (fast-expt-iter base 1 base 1 n))))

(fast-expt-iter 2 1 2 1 1)
(fast-expt-iter 2 1 2 1 2)
(fast-expt-iter 2 1 2 1 3)
(fast-expt-iter 2 1 2 1 4)
(fast-expt-iter 2 1 2 1 5)
(fast-expt-iter 2 1 2 1 6)
(fast-expt-iter 2 1 2 1 7)
(fast-expt-iter 2 1 2 1 8)
(fast-expt-iter 2 1 2 1 9)
(fast-expt-iter 2 1 2 1 10)

(fast-expt-i 2 0)
(fast-expt-i 2 1)
(fast-expt-i 2 2)
(fast-expt-i 2 3)
(fast-expt-i 2 4)
(fast-expt-i 2 5)
(fast-expt-i 2 6)
(fast-expt-i 2 7)
(fast-expt-i 2 8)
(fast-expt-i 2 9)
(fast-expt-i 2 10)

(expt 3 4)
(fast-expt 3 4)
(fast-expt-i 3 4)
(expt 4 2)
(fast-expt 4 2)
(fast-expt-i 4 2)
(expt 9 11)
(fast-expt 9 11)
(fast-expt-i 9 11)

;;; just test : correct when n is powers of 2.
(define (fast-expt-iter-1 acc count n)
  (cond ((= count n) acc)
	((<= (* count 2) n) (fast-expt-iter-1 (square acc) 
					      (* count 2) 
					      n))
	(else (fast-expt-iter-1 acc b 1 (- n count)))))
;;; (fast-expt-iter-1 2 1 8)
;;; -> 2^8 = 256




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 더 좋은 방법 - 별파란님 풀이

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-iter2 b n)
  (define (fast-expt-inner b count product)
    (cond ((= count 0) product)
	  ((even? count) (fast-expt-inner (square b) (/ count 2) product))
	  (else (fast-expt-inner b (- count 1) (* product b)))))
  (fast-expt-inner b n 1))

(fast-expt-iter2 2 0)
(fast-expt-iter2 2 1)
(fast-expt-iter2 2 2)
(fast-expt-iter2 2 3)
(fast-expt-iter2 2 4)
(fast-expt-iter2 2 5)
(fast-expt-iter2 2 6)
(fast-expt-iter2 2 7)
(fast-expt-iter2 2 8)
(fast-expt-iter2 2 9)
(fast-expt-iter2 2 10)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p60
;;; ex 1.17

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recursive process
(define (*-r a b)
  (if (= b 0)
      0
      (+ a (*-r a (- b 1)))))

(*-r 2 0)
(*-r 2 1)
(*-r 2 2)
(*-r 2 3)
(*-r 2 4)
(*-r 2 5)
(*-r 2 6)
(*-r 2 7)
(*-r 2 8)
(*-r 2 9)
(*-r 2 10)



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iterative process
(define (*-i a b) 
  (define (*-iter count acc)
    (if (= count b)
	acc
	(*-iter (+ count 1) (+ acc a))))
  (if (= b 0)
      0
      (*-iter 0 0)))

(*-i 2 0)
(*-i 2 1)
(*-i 2 2)
(*-i 2 3)
(*-i 2 4)
(*-i 2 5)
(*-i 2 6)
(*-i 2 7)
(*-i 2 8)
(*-i 2 9)
(*-i 2 10)




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fast-* : recursive process
(define (even? n)
  (= (remainder n 2) 0))

(define (double x)
  (* x 2))

(define (halves x)
  (/ x 2))

(define (fast-*-r a b)
  (cond ((= b 0) 0)
	((even? b) (double (fast-*-r a (halves b))))
	(else (+ a (fast-*-r a (- b 1))))))

(fast-*-r 2 0)
(fast-*-r 2 1)
(fast-*-r 2 2)
(fast-*-r 2 3)
(fast-*-r 2 4)
(fast-*-r 2 5)
(fast-*-r 2 6)
(fast-*-r 2 7)
(fast-*-r 2 8)
(fast-*-r 2 9)
(fast-*-r 2 10)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p60
;;; ex 1.18
;;; fast-* : iterative process
(define (double x)
  (* x 2))

(define (halves x)
  (/ x 2))

(define (fast-*-iter base acc acc-tmp count n)
  (cond ((= n 1) (+ acc base))
	((= count n) acc)
	((= (double count) n) (+ acc (double acc-tmp)))
	((< (double count) n) (fast-*-iter base
					   acc
					   (double acc-tmp)
					   (double count)
					   n))
	(else (fast-*-iter base
			   (+ acc acc-tmp)
			   base
			   1
			   (- n count)))))

(define (fast-*-i a b)
  (cond ((= b 0) 0)
	(else (fast-*-iter a 0 a 1 b))))

(fast-*-i 2 0)
(fast-*-i 2 1)
(fast-*-i 2 2)
(fast-*-i 2 3)
(fast-*-i 2 4)
(fast-*-i 2 5)
(fast-*-i 2 6)
(fast-*-i 2 7)
(fast-*-i 2 8)
(fast-*-i 2 9)
(fast-*-i 2 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 더 좋은 방법 - 별파란님 풀이
(define (even? n)
  (= (remainder n 2) 0))

(define (double x)
  (* x 2))

(define (halves x)
  (/ x 2))

(define (fast-*-iter2 base acc count)
  (cond ((= count 0) acc)
	((even? count) (fast-*-iter2 (double base) acc (halves count)))
	(else (fast-*-iter2 base
			    (+ acc base)
			    (- count 1)))))
  
(define (fast-*-i2 a b)
  (cond ((= b 0) 0)
	(else (fast-*-iter2 a 0 b))))

(fast-*-i2 2 0)
(fast-*-i2 2 1)
(fast-*-i2 2 2)
(fast-*-i2 2 3)
(fast-*-i2 2 4)
(fast-*-i2 2 5)
(fast-*-i2 2 6)
(fast-*-i2 2 7)
(fast-*-i2 2 8)
(fast-*-i2 2 9)
(fast-*-i2 2 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p61
;;; ex 1.19
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-fib-i n)
  (fast-fib-iter 1 0 0 1 n))

;;  p  q
;;---------------
;;  0  1 - 1 step
;;  1  1 - 2 step
;;  2  3 - 4 step
;; 13 21 - 8 step

;;; longfin님의 방법으로 
(define (fast-fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fast-fib-iter a
			b
			(+ (* p p) (* q q))
			(+ (* q q) (* 2 p q))
			(/ count 2)))
	(else (fast-fib-iter (+ (* b q) (* a q) (* a p))
			     (+ (* b p) (* a q))
			     p
			     q
			     (- count 1)))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)

(fast-fib-i 1)
(fast-fib-i 2)
(fast-fib-i 3)
(fast-fib-i 4)
(fast-fib-i 5)
(fast-fib-i 6)
(fast-fib-i 7)
(fast-fib-i 8)
(fast-fib-i 9)
(fast-fib-i 10)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p63
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p64
;;; ex 1.20
 
;;;;;; normal order -----------------------
(gcd 206 40)
;;->
(if (= 40 0) 
    206 
    (gcd 40 (remainder 206 40)))
;;->
(gcd 40 (remainder 206 40))
;;->
(if (= (remainder 206 40) 0) 
    40 
    (gcd (remainder 206 40)
	 (remainder 40 
		    (remainder (206 40)))))
;;->
;;여기서 처음으로 remainder가 사용됨.
(if (= 6 0)
    40
    (gcd (remainder 206 40)
	 (remainder 40
		    (remainder (206 40)))))





;;;;;; applicative order-----------------------
(gcd 206 40)
;;->
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))
;;->
;;여기서 처음으로 remainder가 사용됨.
(if (= 40 0)
    206
    (gcd 40 6))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p64-p65
(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;; 카마이클 수
(prime? 561) 
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p66-p67

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(expmod 2 1 3)
(expmod 2 2 3)
(expmod 2 3 3)
(expmod 2 5 3)
(expmod 2 7 3)

(expmod 15 1 5)
(expmod 15 2 5)
(expmod 15 3 5)
(expmod 15 5 5)
(expmod 15 7 5)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))


(fast-prime? 561 100) ; 카마이클수
(fast-prime? 562 100) ; not prime
(fast-prime? 1105 100)
(fast-prime? 1729 100)
(fast-prime? 2465 100)
(fast-prime? 2821 100)
(fast-prime? 6601 100)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p68
;;; ex 1.21

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p68
;;; ex 1.22

(define (runtime)
  (current-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond (
	 (< start end) (timed-prime-test start)
	 (search-for-primes (+ start 1) end))))

(search-for-primes 100 1000)
(search-for-primes 1000000 1005000)
(search-for-primes 100000000000 200000000000)

(search-for-primes 1000 10000)
(search-for-primes 100000 1000000)

;;;---------------------
(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;;;---------------------




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p69
;;; ex 1.23
;;; to do


(define (runtime)
  (current-milliseconds))

(define (timed-prime-test2 n)
  (newline)
  (display n)
  (start-prime-test2 n (runtime)))

(define (start-prime-test2 n start-time)
  (if (prime?2 n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

(define (search-for-primes2 start end)
  (cond (
	 (< start end) (timed-prime-test2 start)
	 (search-for-primes2 (+ start 1) end))))

(search-for-primes2 100 1000)
(search-for-primes2 1000000 1005000)
(search-for-primes2 100000000000 100000000100)

(search-for-primes2 1000 10000)
(search-for-primes2 100000 1000000)

;;;---------------------
(define (next test-divisor)
  (if (= test-divisor 2)
      (+ test-divisor 1)
      (+ test-divisor 2)))

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor2 n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime?2 n)
  (= n (smallest-divisor n)))
;;;---------------------


(search-for-primes2 100000000000 100000000060)

(timed-prime-test 100000000003)
(timed-prime-test 100000000019)
(timed-prime-test 100000000057)

(timed-prime-test2 100000000003)
(timed-prime-test2 100000000019)
(timed-prime-test2 100000000057)

(search-for-primes2 1000000000 1000000030)

(timed-prime-test 1000000007)
(timed-prime-test 1000000009)
(timed-prime-test 1000000021)

(timed-prime-test2 1000000007)
(timed-prime-test2 1000000009)
(timed-prime-test2 1000000021)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p70
;;; ex 1.24

(define (runtime)
  (current-milliseconds))

(define (timed-prime-test3 n times)
  (newline)
  (display n)
  (start-prime-test3 n times (runtime)))

;;;
(define (start-prime-test3 n times start-time)
  (if (fast-prime? n times)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

(define (search-for-primes3 start end times)
  (cond ((< start end) (timed-prime-test3 start times)
	 (search-for-primes3 (+ start 1) end) times)))

;;;-------------
(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(fast-prime? 1000000007 100) ; 소수
(fast-prime? 1000000009 100) ; 소수
(fast-prime? 1000000021 100) ; 소수
;;;-------------


(timed-prime-test 1000000007)
(timed-prime-test 1000000009)
(timed-prime-test 1000000021)

(timed-prime-test2 1000000007)
(timed-prime-test2 1000000009)
(timed-prime-test2 1000000021)

(timed-prime-test3 1000000007 100)
(timed-prime-test3 1000000009 100)
(timed-prime-test3 1000000021 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p70
;;; ex 1.25
;;; to do
(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test2 n)
  (define (try-it a)
    (= (expmod2 a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime?2 n times)
  (cond ((= times 0) true)
	((fermat-test2 n) (fast-prime?2 n (- times 1)))
	(else false)))

;;;-------------
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter2 b n)
  (define (fast-expt-inner b count product)
    (cond ((= count 0) product)
	  ((even? count) (fast-expt-inner (square b) (/ count 2) product))
	  (else (fast-expt-inner b (- count 1) (* product b)))))
  (fast-expt-inner b n 1))
;;;-------------

(fast-prime?2 561 100) ; 카마이클 수


; 답은 잘 모르겠다.
; 일단 생각은 expmod에서 fast-expt를 쓰면 안될 것 같은데,, 이유는,,, 쩝
;  단순히 expt를 구하는게 아니라 expt의 mod를 구하고 다시 square하는 과정이
;  중간 결과물에 적용이 되기 때문에 
;  최종 mod 와는 다를 수 있을 것으로 생각된다.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p70
;;; ex 1.26
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;;; exp가 짝수일 때
;;; 기존에서는 expmod를 한 번 평가하고 square를 하는데
;;; 여기서와 같이 작성하면 expmod를 두 번 평가하고 곱한다.
;;; 이 두 번의 평가 속에서 각각 또 두 번의 평가를 수행해야한다.
;;; 그러면 결국 log_2 n의 2 승이 되서 결국 n 과 같게 된다.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p71
;;; ex 1.27

;;; a~n 까지의 수 중에서 a^n mod n 과 a mod n 이 다른 수를 출력 
;;; 카마이클 수이면 아무것도 출력되지 않는다.
(define (test-carmichael a n)
  (define (inner a n count)
    (cond ((< a n)
	   (cond ((not (= (remainder (expt a n) n) (remainder a n)))
		  (print a) ; 맞아떨어지지 않으면 그 수를 출력
		  (newline)
		  (inner (+ a 1) n (+ count 1)))
		 (else (inner (+ a 1) n count))))
	  (else 
	   (if (> count 0)
	       #f
	       #t))))
  (inner a n 0))

(test-carmichael 1 560)
(test-carmichael 1 561)
(test-carmichael 1 1105)
(test-carmichael 1 1729)
(test-carmichael 1 2465)
(test-carmichael 1 2821)
(test-carmichael 1 6601)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p71
;;; ex 1.28
;;; to do


