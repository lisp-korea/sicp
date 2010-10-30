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

