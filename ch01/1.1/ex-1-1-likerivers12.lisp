;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p17
(defun square (x)
  (* x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p23
(defun abs-new (x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p24
(defun abs-new (x)
  (cond ((< x 0) (- x))
	(t x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p25
(defun abs-new (x)
  (if (< x 0)
      (- x)
      (x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p26
(defun >=new (x y)
  (or (> x y) (= x y)))

(defun >=new (x y)
  (not (< x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p28
;;; ex-1-3
(defun square (x)
  (* x x))

(defun f (a b c)
  (cond ((> a b)
	 (+ (square a)
	    (square (if (> b c)
			b
			c))))
	(t 
	 (+ (square b)
	    (square (if (> a c)
			a
			c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p28
;;; ex-1-5
(defun p ()
    (p))

(defun test (x y)
  (if (= x 0)
      0
      y))

;;(test 0 (p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p31
(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun sqrt-sicp (x)
  (sqrt-iter 1.0 x))

(sqrt-sicp 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p33
;;; ex-1-6
(defun square (x)
  (* x x))

(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
	(t else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
;; ok


(defun sqrt-iter (guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

;;=> Infinite call of the sqrt-iter occurs.
;;: Each argument of the new-if should be evaluated
;;: before execution of the new-if's body.
;;: Thus, the sqrt-iter, the argument of the new-if, enters an evaluation stage when the new-if is called.
;;: But the evaluation of the sqrt-iter completes
;;: only when the new-if completes its execution.
;;; The new-if, however, can perform its body and completes when the sqrt-iter,the argument of the new-if, is evaluated.
;;; This situation leads the sqrt-iter's execution infinite.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p34
;;; ex-1-7
(defun square (x)
  (* x x))

(defun average (x y)
  (/ (+ x y) 2))

(defun diff (guess x)
  (abs (- (square guess) x)))

(defun ratio-of-improve (diff-new diff-old)
  (/ diff-new diff-old))

(defun sqrt-iter-ex-1-7 (guess x diff-old)
  (if (good-enough?-ex-1-7 guess x diff-old)
      guess
      (sqrt-iter-ex-1-7 (improve guess x)
			x
			(diff guess x))))

(defun good-enough?-ex-1-7 (guess x diff-old)
  (< (ratio-of-improve (diff guess x)
		       diff-old)
     0.001))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun sqrt-ex-1-7 (x)
  (sqrt-iter-ex-1-7 1.0 x x))

(sqrt-ex-1-7 2)

;;(sqrt-ex-1-7 0.001)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; just difference between diff-new and diffold
(defun good-enough?-ex-1-7 (guess x diff-old)
  (< (abs (- (diff guess x)
	     diff-old))
     0.001))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p34

(defun cube (x)
  (* x x x))

(defun cube-root-iter (guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root-iter (cube-improve guess x) x)))

(defun cube-improve (guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defun cube-good-enough? (guess x)
  (< (abs (- (cube guess) x)) 0.001))

(defun cube-root (x)
  (cube-root-iter 1.0 x))

(cube-root 8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p39
(defun sqrt-sicp (x)
  (labels ((sqrt-iter (guess x)
	     (if (good-enough? guess x)
		 guess
		 (sqrt-iter (improve guess x)
			    x)))
	   (improve (guess x)
	     (average guess (/ x guess)))
	   (average (x y)
	     (/ (+ x y) 2))
	   (good-enough? (guess x)
	     (< (abs (- (square guess) x)) 0.001))))
  (sqrt-iter 1.0 x))

(sqrt-sicp 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p40
(defun square (x)
  (* x x))

(defun average (x y)
  (/ (+ x y) 2))

(defun sqrt-sicp (x)
  (labels ((sqrt-iter (guess)
	     (if (good-enough? guess)
		 guess
		 (sqrt-iter (improve guess))))

	   (improve (guess)
	     (average guess (/ x guess)))

	   (good-enough? (guess)
	     (< (abs (- (square guess) x)) 0.001)))

    (sqrt-iter 1.0)))

(sqrt-sicp 2)
