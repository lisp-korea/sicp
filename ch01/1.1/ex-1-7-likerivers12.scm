
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

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough?-ex-1-7 guess x diff-old)
  (< (ratio-of-improve (diff guess x)
		       diff-old)
     0.001))

(define (sqrt-ex-1-7 x)
  (sqrt-iter-ex-1-7 1.0 x x))


(define (run-test)
  (print (sqrt-ex-1-7 2))
  (newline)
  (print (sqrt-ex-1-7 0.0000001))
  (newline)
)
