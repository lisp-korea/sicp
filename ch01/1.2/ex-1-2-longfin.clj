;;ex 1.9

;; (recur-add 3 5)
;; (inc (recur-add 2 5))
;; (inc (inc (recur-add 1 5)))
;; (inc (inc (inc (recur-add 0 5))))
;; (inc (inc (inc 5)))
;; (inc (inc 6))
;; (inc 7)
;; 8
(defn recur-add [a b]
  (if (= a 0)
    b
    (inc (recur-add (dec a) b))))

;; (tail-recur-add 3 5)
;; (tail-recur-add 2 6)
;; (tail-recur-add 1 7)
;; (tail-recur-add 0 8)
;; 8
(defn tail-recur-add [a b]
  (if (= a 0)
    b
    (tail-recur-add (dec a) (inc b))))

;;ex 1.10

(defn A [x y]
  (cond (= y 0) 0
	(= x 0) (* 2 y)
	(= y 1) 2
	:else (A (- x 1)
		 (A x (- y 1)))))

;; user> (A 1 10)
;; (A 0 (A 1 9))
;; (* 2 (A 1 9))
;; (* 2 (A 0 (A 1 8)))
;; (* 2 (* 2 (A 0 (A 1 7))))
;; (* 2 (* 2 (* 2 (A 0 (A 1 6)))))
;; ...
;; 1024
;; user> (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (* 2 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (* 2 (* 2 (* 2 2))))
;; (A 1 16)
;; (A 0 (A 1 15))
;; ...
;; (A 0 (A 0 (A 1 14)))
;; ...
;; 65536
;; user> (A 3 3)
;; 65536

(defn f [n]
  (A 0 n))

;; (f 3)
;; (A 0 3)
;; (* 2 3)
;; (f n) => 2n

(defn g [n]
  (A 1 n))

;; (g 3)
;; (A 1 3)
;; (A 0 (A 1 2)) == (A 0 (g 2))
;; (* 2 (A 0 (A 1 1))) == (* 2 (A 0 (g 1)))
;; (* 2 (* 2 2))
;; 8
;; (g n) => 2^n
 
(defn h [n]
  (A 2 n))
;; (h 3)
;; (A 2 3)
;; (A 1 (A 2 2))
;; (g (A 2 2)) == (g (h 2))
;; (g (A 1 (A 2 1)))
;; (g (g (A 2 1))) == (g (g (h 1)))
;; (g (g 2))
;; (g 4)
;; 16
;; (h n) => ?

;; ex 1.11
;; f(0) => 0
;; f(1) => 1
;; f(2) => 2
;; f(3) => f(2) + 2f(1) + 3f(0) => 2 + 2*1 + 3*0
;; f(4) => f(3) + 2f(2) + 3f(1) => 4 + 2*2 + 3*1
;; f(5) => f(4) + 2f(3) + 3f(2) => 11 + 2*3 + 3*2
(defn f-recur [n]
  (if (< n 3)
    n
    (+ (f-recur (- n 1))
       (* 2 (f-recur (- n 2)))
       (* 3 (f-recur (- n 3))))))

;; count = 0: a, b, c = f(0), f(1), f(2) 
;; count = 1: a, b, c = f(1), f(2), f(3)
;; count = 2: a, b, c = f(2), f(3), f(4)
;; count = 3: a, b, c = f(3), f(4), f(5)
;; count = 4: a, b, c = f(4), f(5), f(6)
(defn f-iter [n]
  (letfn [(_iter [a b c count]
		 (if (= count n)
		   a
		   (_iter
		    b
		    c
		    (+ c (* 2 b) (* 3 a))
		    (+ count 1)))
		 )]
    (_iter 0 1 2 0)))

;; ex 1.12
;; solve 1
(defn pascal [x y]
  (cond (= x y) 1
	(= x 0) 1
	:else (+ (pascal (dec x) (dec y))
		 (pascal x (dec y)))))
(defn pascal-triangle [n]
  (letfn [(_iter-per-col [r c length line]
			 (if (= c length)
			   line
			   (_iter-per-col r
					  (inc c)
					  length
					  (conj line (pascal c r)))))
	  (_iter-per-row [l result]
			 (if (> l n)
			     result
			     (_iter-per-row
			      (inc l)
			      (conj result (_iter-per-col l 0 (inc l) [])))))]
    (_iter-per-row 0 [])))

;;solve 2
(defn pascal-row[n]
  (letfn [(next-row [row]
		    (letfn [(_iter [c nrow]
				   (if (= c (dec (count row)))
				     nrow
				     (_iter (inc c)
					    (conj nrow
						  (+ (nth row c)
						     (nth row (inc c)))))))]
		      (conj (vec (cons 1 (_iter 0 []))) 1)
		      ))]
    (cond (= n 0) [1]
	  (= n 1) [1 1]
	  :else (next-row (pascal-row (dec n))))))
(defn pascal-triangle [n]
  (letfn [(_iter-row [c result]
		     (if (= c n)
		       result
		       (_iter-row (inc c) (conj result (pascal-row c)))))]
    (_iter-row 0 [])))

;; Ex 1.15
(defn cube [x]
  (* x x x))
(defn p [x]
  (- (* 3 x)
     (* 4 (cube x))))
(defn abs [n]
  (if (< n 0)
    (* -1 n)
    n))
(defn sine [angle]
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

;; (sine 12.15)
;; (p (sine (/ 12.15 3.0)))
;; (p (sine 4.05))
;; (p (p (sine (/ 4.05 3.0))))
;; (p (p (sine 1.35)))
;; (p (p (p (sine (/ 1.35 3.0)))))
;; (p (p (p (p (sine 0.45)))))
;; (p (p (p (p 0.45))))

(defn expt [b n]
  (letfn [(_iter [b counter product]
		 (if (= counter 0)
		   product
		   (_iter
		    b
		    (- counter 1)
		    (* b product))))]
    (_iter b n 1)))

(defn square [n]
  (* n n))
(defn fast-expt [b n]
  (cond (= n 0) 1
	(even? n) (square (fast-expt b (/ n 2)))
	:else (* b (fast-expt b (- n 1)))))
;; ex1.16
(defn fast-expt-iter [b n]
  (letfn [(_iter [b counter product]
		 (cond
		  (= counter 0) product
		  (even? counter) (_iter (* b b)
					 (/ counter 2)
					 product)
		  :else (_iter b
			       (- counter 1)
			       (* product b))))]
    (_iter b n 1)))

;; ex1.17

(defn my* [a b]
  (if (= b 0)
    0
    (+ a (my* a (- b 1)))))
(defn twice [n]
  (* 2 n))
(defn halve [n]
  (/ n 2))

(defn fast-my* [a b]
  (cond
   (= b 0) 0
   (even? b) (fast-my* (twice a) (halve b)) 
   :else (+ a (fast-my* a (- b 1)))))

;; ex1.18

(defn fast-my*-iter [a b]
  (letfn [(_iter[a b result]
		(cond
		 (= b 0) result
		 (even? b) (_iter (twice a) (halve b) result)
		 :else (_iter a (dec b) (+ result a))
		 ))]
    (_iter a b 0)
    ))

;; ex1.19

;; Tpq
;; a = bq + aq + ap
;; b = bp + aq

;; Tpq^2
;; a = (bp+aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;   = bpq + aqq + bqq + aqq + apq + bpq + apq + app
;;   = 2bpq + bqq + 2apq + app + aqq + aqq
;;   = b(2pq + qq) + a(2pq + qq) + a(pp + qq)
;; b = (bp+aq)p + (bq + aq + ap)q
;;   = bpp + apq + bqq + aqq + apq
;;   = 2apq + aqq + bpp + bqq
;;   = b(pp + qq) + a(2pq + qq)

;; p' = (pp + qq)
;; q' = (2pq + qq)
(defn fib [n]
  (letfn [(_iter [a b p q count]
		 (cond (= count 0) b
		       (even? count) (_iter a
					    b
					    (+
					     (* p p)
					     (* q q))
					    (+
					     (* 2 p q)
					     (* q q))
					    (/ count 2))
		       :else (_iter (+ (* b q) (* a q) (* a p))
				    (+ (* b p) (* a q))
				    p
				    q
				    (- count 1))))]
    (_iter 1 0 0 1 n)))

(defn % [a b]
  (mod a b))
    
(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (% a b))))

;; ex 1.20
;;
;; a. normal order
;; (gcd 206 40)
;; a = 206
;; b = 40
;; (if (= (% 206 40) 0)
;;   206
;;   ;; a = 40
;;   ;; b = (% 206 40) = 6
;;   (if (= (% 206 40) 0)
;;     40
;;     ;; a = (% 206 40) = 6
;;     ;; b = (% 40 (% 206 40)) = 4
;;     (if (= (% 40 (% 206 40)) 0)
;;       (% 206 40)
;;       ;; a = (% 40 (% 206 40)) = 4
;;       ;; b = (% (% 206 40) (% 40 (% 206 40))) = 2
;;       (if (= (% (% 206 40) (% 40 (% 206 40))) 0) 
;; 	(% 40 (% 206 40))
;; 	;; a = (% (% (206 40) (% 40 (% 206 40)))) = 2
;; 	;; b = (% (% 40 (% 206 40)) (% (% (206 40) (% 40 (% 206 40))))) = 0
;; 	(if (= (% (% 40 (% 206 40)) (% (% (206 40) (% 40 (% 206 40))))) 0)
;; 	  (% (% (206 40) (% 40 (% 206 40))))
;; 	  (gcd ...))))))
;; 19 times?

;; b. applicative order
;; (gcd 206 40)
;; (gcd (gcd 40 (% 206 40)))
;; (gcd (gcd 40 6))
;; (gcd (gcd (gcd 6 (% 40 6))))
;; (gcd (gcd (gcd 6 4)))
;; (gcd (gcd (gcd 4 (% 6 4))))
;; (gcd (gcd (gcd 4 2)))
;; (gcd (gcd (gcd 2 (% 2 4))))
;; (gcd (gcd (gcd 2 0)))
;; 2
;; 4times

(defn divides? [a b]
  (= (% b a) 0))
(defn find-divisor [n test-divisor]
  (loop [n n
	 test-divisor test-divisor]
    (cond (> (square test-divisor) n) n
	  (divides? test-divisor n) test-divisor
	  :else (recur n (+ test-divisor 1)))))
(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn expmod [base exp m]
  (cond (= exp 0) 1
	(even? exp) (% (square (expmod base (/ exp 2) m))
		       m)
	:else (% (* base (expmod base (- exp 1) m))
		 m)))

;; ex 1.21
(smallest-divisor 199) ;199
(smallest-divisor 1999) ;1999
(smallest-divisor 19999) ;7


;; ex 1.22
(defn report-prime [elapsed-time]
  (print "***")
  (print elapsed-time)
  (newline))
(defn start-prime-test [n start-time]
  (if (prime? n)
    (report-prime (- (System/currentTimeMillis) start-time))))
(defn timed-prime-test [n]
  (newline)
  (print n)
  (start-prime-test n (System/currentTimeMillis)))

(defn search-primes [min count]
  (loop [m min
	 c count]
    (cond (= c 0) (newline)
	  (prime? m) (do
		       (timed-prime-test m)
		       (recur (+ 2 m) (dec c)))
	  :else (recur (+ 2 m) c))))

;; ex 1.23
(defn next-divisor [n]
  (if (= n 2)
    3
    (+ 2 n)))

;; ex 1.24
(defn fermat-test [n]
  (letfn [(try-it [a]
		  (= (expmod a n n) a))]
    (try-it (inc (rand-int (dec n))))))

(defn fast-prime? [n count]
  (loop [n n count count]
    (cond (= count 0) true
	  (fermat-test n) (recur n (dec count))
	  :else false)))

;; ex 1.27

(defn carmichael? [n]
  (loop [a 1]
    (cond (= n a) true
	  (= (expmod a n n) (% a n)) (recur (inc a))
	  :else false)))

;; ex 1.28
(defn expmod [base exp n]
  (cond (= exp 0) 1
	(even? exp)
	(let
	    [root (expmod base (/ exp 2) n)
	     next (% (square root) n)]
	  (if (and
	       (not (= root 1))
	       (not (= root (dec n)))
	       (= next 1))
	    0
	    next))
	:else (% (* base (expmod base (dec exp) n))
		 n)))