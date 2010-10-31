
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



(define (run-test)
  (print (cube-root 8))
  (newline)
  (print (cube-root 0.000001))
  (newline)
)
