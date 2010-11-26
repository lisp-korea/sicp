(define tolerance 0.00001)
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
	  

;; ex 1.45
(define (nth-root-test x n r)
  (fixed-point
   ((repeated average-damp r) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

;; r=1(n < 3)

;; > (nth-root-test (expt 2 2) 2 1)
;; 2.000000000000002
;; > (nth-root-test (expt 2 3) 3 1)
;; 1.9999981824788517
;; > (nth-root-test (expt 2 4) 4 1)
;;   ^C ^Cuser break

;; r=2(n < 7)

;; > (nth-root-test (expt 2 4) 4 2)
;; 2.0000000000021965
;; > (nth-root-test (expt 2 5) 5 2)
;; 2.000001512995761
;; > (nth-root-test (expt 2 6) 6 2)
;; 2.0000029334662086
;; > (nth-root-test (expt 2 7) 7 2)
;; 2.0000035538623377
;; > (nth-root-test (expt 2 8) 8 2)
;;   ^C ^Cuser break


(define (log2 n)
  (/ (log n)
     (log 2)))
(define (nth-root x n)
  (fixed-point
   ((repeated average-damp (floor (log2 n))) (lambda (y) (/ x (expt y (- n 1))))) 1))
   
;; ex 1.46

(define (iterative-improve good-enough? improve)
  (lambda (g)
    (define (try g)
      (let ((improved (improve g)))
	(if (good-enough? g improved)
	    improved
	    (try improved))))
    (try g)))

(define (sqrt x)
  ((iterative-improve
    (lambda (v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (lambda (y)
      (average y (/ x y)))) 1.0))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (v1 v2)
      (< (abs (- v1 v2)) tolerance))
    f) first-guess))