;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p17
(define (square x) (* x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p28
;;; ex-1-3
(define (square x) (* x x))

(define (f a b c)
  (cond ((> a b) 
	 (+ (square a)
	    (square (if (> b c)
			b
			c))))
	(else
	 (+ (square b)
	    (square (if (> a c)
			a
			c))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p28
;;; ex-1-5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p31
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-sicp x)
  (sqrt-iter 1.0 x))

;;
(sqrt-sicp 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p33
;;; ex-1-6
(define (square x) (* x x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
;; ok

(define (sqrt-iter guess x)
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
(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (diff guess x)
  (abs (- (square guess) x)))

(define (ratio-of-improve diff-new diff-old)
  (/ diff-new diff-old))

(define (sqrt-iter-ex-1-7 guess x diff-old)
  ;; (print x)
  ;; (print "")
  ;; (print guess)
  ;; (print "")
  ;; (print diff-old)
  ;; (newline)
  (if (good-enough?-ex-1-7 guess x diff-old)
      guess
      (sqrt-iter-ex-1-7 (improve guess x)
			x
			(diff guess x))))


(define (good-enough?-ex-1-7 guess x diff-old)
  (< (ratio-of-improve (diff guess x)
		       diff-old)
     0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-ex-1-7 x)
  (sqrt-iter-ex-1-7 1.0 x x))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; just difference between diff-new and diffold
(define (good-enough?-ex-1-7 guess x diff-old)
  (< (abs (- (diff guess x)
	     diff-old))
     0.001))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p34
;;; ex-1-8
(define (cube x)
  (* x x x))

(define (cube-root-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root-iter (cube-improve guess x) x)))

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube-root x)
  (cube-root-iter 1.0 x))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p39

(define (sqrt x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x)
		   x)))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

  (sqrt-iter 1.0 x))

;;
(sqrt 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p40
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
