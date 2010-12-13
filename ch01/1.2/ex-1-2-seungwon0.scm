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
