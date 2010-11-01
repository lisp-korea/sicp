;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p17
(defn square [x]
  (* x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p23
(defn abs [x]
  (cond (> x 0) x
	(= x 0) 0
	(< x 0) (- x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p24
(defn abs [x]
  (cond (< x 0) (- x)
	true x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p25
(defn abs [x]
  (if (< x 0)
      (- x)
      x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p26
(defn >=new [x y]
  (or (> x y) (= x y)))

(defn >=new [x y]
  (not (< x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p28
;;; ex-1-3
(defn square [x]
  (* x x))

(defn f [a b c]
      (cond (> a b)
	    (+ (square a)
	       (square (if (> b c)
			   b
			   c)))
	    true 
	    (+ (square b)
	       (square (if (> a c)
			   a
			   c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p28
;;; ex-1-5
(defn p []
  (p))

(defn test1 [x y]
      (if (= x 0)
	  0
	  y))

;;(test1 0 (p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p31
(defn abs [x]
  (if (< x 0)
      (- x)
      x))

(defn improve [guess x]
      (average guess (/ x guess)))

(defn average [x y]
      (/ (+ x y) 2.0))

(defn good-enough? [guess x]
      (< (abs (- (square guess) x)) 0.001))

;; (defn sqrt-iter [guess x]
;;       (if (good-enough? guess x)
;; 	  guess
;; 	  (recur (improve guess x)
;; 		     x)))

(defn sqrt-iter [guess x]
      (if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
			     x)))

(defn sqrt-sicp [x]
  (sqrt-iter 1.0 x))

(sqrt-sicp 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p33
;;; ex-1-6
(defn square [x]
  (* x x))

(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
	true else-clause))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
;; ok

;;; works on clojure
(defn sqrt-iter [guess x]
      (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p34
;;; ex-1-7
(defn square [x]
  (* x x))

(defn average [x y]
  (/ (+ x y) 2))

(defn abs [x]
  (if (< x 0)
      (- x)
      x))

(defn diff [guess x]
  (abs (- (square guess) x)))

(defn ratio-of-improve [diff-new diff-old]
  (/ diff-new diff-old))

(defn good-enough?-ex-1-7 [guess x diff-old]
  (< (ratio-of-improve (diff guess x)
		       diff-old)
     0.001))

(defn sqrt-iter-ex-1-7 [guess x diff-old]
  (if (good-enough?-ex-1-7 guess x diff-old)
      guess
      (sqrt-iter-ex-1-7 (improve guess x)
			x
			(diff guess x))))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-ex-1-7 [x]
  (sqrt-iter-ex-1-7 1.0 x x))

(sqrt-ex-1-7 2)

;;(sqrt-ex-1-7 0.001)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; just difference between diff-new and diffold
(defn good-enough?-ex-1-7 [guess x diff-old]
  (< (abs (- (diff guess x)
	     diff-old))
     0.001))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p34

(defn cube [x]
  (* x x x))

(defn abs [x]
  (if (< x 0)
      (- x)
      x))

(defn square [x]
  (* x x))

(defn cube-good-enough? [guess x]
  (< (abs (- (cube guess) x)) 0.001))

(defn cube-root-iter [guess x]
  (if (cube-good-enough? guess x)
      guess
      (cube-root-iter (cube-improve guess x) x)))

(defn cube-improve [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))


(defn cube-root [x]
  (cube-root-iter 1.0 x))

(cube-root 8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p39
(defn sqrt-sicp [x]
  (letfn [(sqrt-iter [guess x]
		     (if (good-enough? guess x)
			 guess
			 (sqrt-iter (improve guess x)
				    x)))
	 (improve [guess x]
		  (average guess (/ x guess)))
	 (average [x y]
		  (/ (+ x y) 2))
	 (good-enough? [guess x]
		       (< (abs (- (square guess) x)) 0.001))]

		       (sqrt-iter 1.0 x)))

(sqrt-sicp 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p40
(defn square [x]
  (* x x))

(defn average [x y]
  (/ (+ x y) 2))

(defn abs [x]
  (if (< x 0)
      (- x)
      x))

(defn sqrt-sicp [x]
  (letfn [(sqrt-iter [guess]
		     (if (good-enough? guess)
			 guess
			 (sqrt-iter (improve guess))))

	 (improve [guess]
		  (average guess (/ x guess)))

	 (good-enough? [guess]
	     (< (abs (- (square guess) x)) 0.001))]

	     (sqrt-iter 1.0)))

(sqrt-sicp 2)
