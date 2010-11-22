(define tolerance 0.0001)
(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2.0))
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y)  (/ x y)))
	       1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x)
  (* x x x))

((deriv cube) 5)

(define (newtons-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; using average-damp
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))

;; using newtons-method
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newtons-method
			    1.0))

;; ex 1.40

(define (cubic a b c)
  (lambda (x)
    (+
     (* x x x)
     (* a x x)
     (* b x)
     c)))

;; ex 1.41

(define (inc x)
  (+ x 1))
(define (double f)
  (lambda(x)
    (f (f x))))

(((double (double double)) inc) 5)

;; ex 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)

;; ex 1.43

(define (repeated f n)
  (define (repeated-iter applied c)
    (if (= c n)
	applied
	(repeated-iter (compose f applied) (+ c 1))))
  (repeated-iter f 1))

;; ex 1.44

(define (smooth f)
  (lambda (x)
    (/ (+
	(f (+ x dx))
	(f x)
	(f (- x dx)))
       3.0)))

(define (smooth-n f n)
  (repeated (smooth f) n))
	  
	    
	      
	      
	      