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

(fast-expt-iter-3 2 1 2 1 1)
(fast-expt-iter-3 2 1 2 1 2)
(fast-expt-iter-3 2 1 2 1 3)
(fast-expt-iter-3 2 1 2 1 4)
(fast-expt-iter-3 2 1 2 1 5)
(fast-expt-iter-3 2 1 2 1 6)
(fast-expt-iter-3 2 1 2 1 7)
(fast-expt-iter-3 2 1 2 1 8)
(fast-expt-iter-3 2 1 2 1 9)
(fast-expt-iter-3 2 1 2 1 10)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p61
;;; ex 1.19
;;; to do
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-fib-i n)
  (fast-fib-iter 1 0 0 1 n))

;;  p  q
;;---------------
;;  0  1 - 1 step
;;  1  2 - 2 step
;;  2  3 - 4 step
;; 13 21 - 8 step
(define (fast-fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fast-fib-iter a
			b
			< > 
			< > 
			(/ count 2)))
	(else (fast-fib-iter (+ (* b q) (* a q) (* a p))
			     (+ (* b p) (* a q))
			     p
			     q
			     (- count 1)))))


(define (fib-iter2 a b p q count n)
  (cond ((= count (+ n 1)) b)
	(else (fib-iter2 (+ (* b q) (* a q) (* a p))
			 (+ (* b p) (* a q))
			 p
			 q
			 (+ count 1)
			 n))))

(define (fib-iter3 a b p q count)
  (cond ((= count 0) b)
	(else (fib-iter3 (+ (* b q) (* a q) (* a p))
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
;;; to do




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

;;; I think.
;;; needs not operator to predict true when n is a prime number.
(define (prime? n)
  (not (= n (smallest-divisor n))))

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p66-p67
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

(define (square x)
  (* x x))

(fast-prime? 10 562) ;; not prime
(fast-prime? 10 561) ;; prime

(fast-prime? 100 100) ;; not prim
(fast-prime? 100 562) ;; not prime
(fast-prime? 100000000 561) ;; prime

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p68
;;; ex 1.21
;;; to do



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p68
;;; ex 1.22
;;; to do


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p69
;;; ex 1.23
;;; to do



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p70
;;; ex 1.24
;;; to do




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p70
;;; ex 1.25
;;; to do





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p70
;;; ex 1.26
;;; to do





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p71
;;; ex 1.27
;;; to do




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p71
;;; ex 1.28
;;; to do


