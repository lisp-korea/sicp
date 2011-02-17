;; by byulparan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inc a)
  (+ a 1))

(define (dec a)
  (- a 1))

(define (recur+ a b)
  (if (= a 0)
      b
      (inc (recur+ (dec a) b))))

;; (recur+ 5 4)
;; (inc (recur+ 4 4))
;; (inc (inc (recur+ 3 4)))
;; (inc (inc (inc (recur+ 2 4))))
;; (inc (inc (inc (inc (recur+ 1 4)))))
;; (inc (inc (inc (inc (inc (recur+ 0 4))))))
;; (inc (inc (inc (inc (inc 4)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

(define (tail-recur+ a b)
  (if (= a 0)
      b
      (tail-recur+ (dec a) (inc b))))

;; (tail-recur+ 5 4)
;; (tail-recur+ 4 5)
;; (tail-recur+ 3 6)
;; (tail-recur+ 2 7)
;; (tail-recur+ 1 8)
;; (tail-recur+ 0 9)
;; 9


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))


(A 1 10) ; => 1024
(A 2 4) ; => 65536
(A 3 3) ; => 65536


(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

(f 2)
;=> 4
(f 3)
;=> 6
(f 10)
;=> 20
(f 45)
;=> 90

; f(n) =  n * 2

(g 2)
;=> 4
(g 3)
;=> 8
(g 4)
;=> 16

(g 3)
;=> (A 1 3)
;=>(A 0 (A 1 2))
;=>(* 2 (A 0 (A 1 1)))
;=>(* 2 (* 2 2))
;=> (* 2 2 2)
;=> 2^3
(g 4)
;=> (A 1 4)
;=> (A 0 (A 1 3))
;=> (* 2 (A 0 (A 1 2)))
;=> (* 2 2 (A 0 (A 1 1)))
;=> (* 2 2 2 2)
;=> 2 ^ 4

; g(n) = 2 ^ n


(h 1)
;=> 2
(h 2)
;=> 2^h(1) = 4
(h 3)
;=> 2^h(2) = 16
(h 4)
;=> 2^h(3) = 2^16 = 65536
(h 5)
;=> 2^h(4) = 2^65536

(h 1)
; (A 2 1)
; 2

(h 2)
; (A 2 2)
; (A 1 (A 2 1))
; (A 1 (h 1))
;; (A 1 n) == g(n) == 2^n
;; (A 1 (h 1)) == 2^(h 1)

(h 3)
; (A 2 3)
; (A 1 (A 2 2))
; (A 1 (h 2)) == 2^(h 2)

(h 4)
; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (h 3)) ==  2^(h 3)


;; h(1) = 2
;; h(n) = 2 ^ h(n - 1)
;; h(4) == 2 ^ h(3)  == (expt 2 (h 3))
;; (= (expt 2 (h 3)) (h 4))

;; h(5) ==  2 ^ h(4)
;; (= (expt 2 (h 4)) (h 5))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.11 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; n < 3 == f(n) = n
;; n >= 3 == f(n) = f(n-1) + 2f(n - 2) + 3f(n - 3)

(define (fibo n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fibo (- n 1))
		 (fibo (- n 2))))))


(define (fibo-iter a b count)
  (cond ((= count 0) b)
	(else (fibo-iter (+ a b) a (- count 1)))))


       (fibo 4)
  (fibo 3)  (fibo 2)

       (fibo 5)
(+ (fibo 4) (fibo 3))



(define (f n)
  (cond ((> 3 n) n)
	(else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

;;
;;
;;                              f(3)
;;                 (+ (f 2)  (* 2 (f 1))  (* 3 (f 0))) 
;;                  
;;
;;                              f(4)
;;              (+ (f 3) (* 2 (f 2)) (* 3 (f 1)))                    
;;
;;                              f(5)
;;           (+ (f 4)       (* 2 (f 3))       (* 3 (f 2))
;;
;;                              f(6)
;;             (+  (f 5)    (* 2 (f 4))    (* 3 (f 3)))
;;
;;
(define (f-iter n)
  (define (f-inner-iter a b c count)
    (cond ((= count 0) c)
	  (else (f-inner-iter (+ a (* b 2) (* c 3)) a b (- count 1)))))
  (f-inner-iter 2 1 0 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.12 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   --------------------->  x              pascal(0,0)  = 1
;;   |           1                          pascal(1,4)  = 4
;;   |         1   1                        pascal(4,4)  = 1
;;   |       1   2   1                      pascal(0,5)  = 1
;;   |     1   3   3   1                    pascal(1,5)  = 5
;;   |   1   4   6   4   1                  pascal(5,5)  = 1
;;   | 1   5   10  10  5   1                x == 0 then 1   
;;   y                                      x == y then 1
;;
;;   pascal(1,5)  = pascal(0,4) + pascal(1,4)
;;   pascal(1,4)  = pascal(0,3) + pascal(1,3)
;;   pascal(1,3)  = pascal(0,2) + pascal(1,2)
;;   pascal(1,2)  = pascal(0,1) + pascal(1,1)
;;
;;   pascal(1,5)  = pascal(0,4) + pascal(0,3) + pascal(0,2) + pascal(0,1) + pascal(1,1) = 5
;;
;;
;;   pascal(2,4) = pascal(1,3) + pascal(2,3)
;;   pascal(2,4) = 3 + pascal(2,3)
;;
;;   pascal(2,3) = pascal(1,2), + pascal(2,2)
;;                      2              1
;;   pascal(2,4) = 3 + 2 + 1 = 6
;;
;;   pascal(x,y)  ->   x == 0 then 1, x == y then 1
;;                ->   pascal(x-1,y-1) + pascal(x,y-1)

(define (pascal x y)
  (cond ((= x 0) 1)
	((= x y) 1)
	(else (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1))))))


(define (display-pascal n)
  (define (pascal-inner n count)
    (cond ((> count n) (display "\n"))
	  (else (display (pascal count n))
		(display "  ")
		(pascal-inner n (+ 1 count)))))
  (define (pascal-iter n count)
    (cond ((= count n) (display "\n"))
	  (else (pascal-inner count 0)
		(pascal-iter n (+ count 1)))))
  (pascal-iter n 0))
	      
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.13 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ...-_-??


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.14 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
	  ((or (< amount 0) (= kinds-of-coins 0)) 0)
	  (else (+ (cc amount (- kinds-of-coins 1))
		   (cc (- amount (first-denomination kinds-of-coins))
		       kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 5)
	  ((= kinds-of-coins 3) 10)
	  ((= kinds-of-coins 4) 25)
	  ((= kinds-of-coins 5) 50)))
  (cc amount 5))

;; 각자 그려보자.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.15 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))


; (sine 12.15)
; (p (sine 4.05))
; (p (p (sine 1.3499999999999)))
; (p (p (p (sine 0.449999999996))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine 0.04999999999996))))))

(define (count-call-p n)
  (define (count-call-p-inner n count)
    (if (> 0.1 (abs n)) count
	(count-call-p-inner (/ n 3.0) (+ count 1))))
  (count-call-p-inner n 0))

(count-call-p 12.15)
; => 5

(count-call-p 1000)
; => 9



;; 1.2.4 거듭제곱
(define (expt b n)
  (if (= n 0) 1
      (* b (expt b (- n 1)))))

;; (expt 2 4)
;; (* 2 (expt 2 3))
;; (* 2 (* 2 (expt 2 2)))
;; (* 2 (* 2 (* 2 (expt 2 1))))
;; (* 2 (* 2 (* 2 (* 2 (expt 2 0)))))
;; (* 2 2 2 2 1)

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0) product
      (expt-iter b (- counter 1) (* b product))))

; (expt 2 4)
; (expt-iter 2 4 1)
; (expt-iter 2 3 2)
; (expt-iter 2 2 4)
; (expt-iter 2 1 8)
; (expt-iter 2 0 16)
; 16

(define (square n)
  (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else ( * b (fast-expt b (- n 1))))))

;; (fast-expt 2 4)
;; (square (fast-expt 2 2))
;; (square (square (fast-expt 2 1)))
;; (square (square (* 2 (fast-expt 2 0))))
;; (square (square (* 2 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.16 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fast-expt-iter b n)
  (define (fast-expt-iter-inner b n product)
    (cond ((= n 0) product)
	  ((even? n) (fast-expt-iter-inner (square b) (/ n 2) product))
	  (else (fast-expt-iter-inner b (- n 1) (* product b)))))
  (fast-expt-iter-inner b n 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.17 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my* a b)
  (if (= b 0) 0
      (+ a (my* a (- b 1)))))

(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))
      

(define (fast-* a b)
  (cond ((= b 0) 0)
	((even? b) (double (fast-* a (halve b))))
	(else (+ a (fast-* a (- b 1))))))


(fast-* 3 10)
(fast-* 21 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.18 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fast-iter-* a b)
  (define (fast-iter-*-inner a b product)
    (cond ((= b 0) product)
	  ((even? b) (fast-iter-*-inner (double a) (halve b) product))
	  (else (fast-iter-*-inner a (- b 1) (+ product a)))))
  (fast-iter-*-inner a b 0))

(if (and
     (= (fast-iter-* 4 5) (* 4 5))
     (= (fast-iter-* 12 7) (* 12 7))
     (= (fast-iter-* 8 36) (* 8 36))
     (= (fast-iter-* 51 36) (* 51 36)))
    "OK"
    "NO GOOD")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.19 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.20 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (% a b)
  (remainder a b))

(gcd 206 40)


;; 정의대로 계산 시 
(if (= 40 0)                      ;; 정의대로 계산하더라도 if 는 더 이상 미룰 수 없고 즉시 계산되어야 한다.
    206
    (gcd 40 (remainder 206 40)))


(if (= (% 206 40) 0) 		; 6
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(if (= (% 40 (% 206 40)) 0)  ; 4
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))


(if (= (% (% 206 40) (% 40 (% 206 40))) 0)  ; 2
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	 (remainder (remainder 40 (remainder 206 40))
		    (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

          
(if (= (% (% 40 (% 206 40))
		  (% (% 206 40) (% 40 (% 206 40)))) 0) ; 0
    (% (% 206 40) (% 40 (% 206 40))) ;;=> 2
    (gcd (remainder (remainder 40 (remainder 206 40))
		    (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
	 (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
		    (remainder (remainder 40 (remainder 206 40))
			       (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))


;; remainder 는 if 문이 평가될때, 그리고 평가결과가 참일때 리턴되면서 계산된다.
;; 18 번 호출


;; 인자 먼저 계산 시
(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (% 206 40)))

(if (= 6 0)
    40
    (gcd 6 (% 40 6)))

(if (= 4 0)
    6
    (gcd 4 (% 6 4)))

(if (= 2 0)
    4
    (gcd 2 (% 4 2)))

(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))

;; 4 번 호출


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.21 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square n)
  (* n n))

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

(smallest-divisor 199)
;=> 199
(smallest-divisor 1999)
;=> 1999
(smallest-divisor 19999)
;=> 7


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.22 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (runtime)
  (current-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (void)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (if (even? start) (search-for-primes (+ start 1) end)
      (cond ((> end start) (timed-prime-test start)
	                   (search-for-primes (+ start 2) end)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.23 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next n)
  (if (= n 2) 3 (+ n 2)))


(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))


;; old-version
;1000000000039 *** 681
;1000000000061 *** 624
;1000000000063 *** 624

;; new-viersion
;1000000000039 *** 331
;1000000000061 *** 321
;1000000000063 *** 320


;; 음..대략 두배는 빨라지는데..=_=??

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.24 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

