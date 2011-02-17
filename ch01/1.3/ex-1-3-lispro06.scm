;ex 1.40
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs ( - v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
         next
         (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
      (* a (* x x))
      (* b x)
      c)))
(print "ex 1.40")
(newline)
(newtons-method (cubic -6 3 10) 1)
(newtons-method (cubic 4 6 2) 1)
(newtons-method (cubic 1 -4 -10) 1)
;ex 1.41
(define (inc a)
  (+ 1 a)
)
 (define (double f) 
         (lambda (x) (f (f x)))) 
 
(print "ex 1.41")
(newline)
 (((double (double double)) inc) 5)
 
 ;ex 1.42
 (define (square x)
   (* x x)
   )
  (define (compose f g) 
    (lambda (x) (f (g x)))) 
 
 (print "ex 1.42")
 (newline)
 ((compose square inc) 6)
 
 ;ex 1.43
(define (repeat f n) 
  (lambda (x) 
    (if (< n 1) x 
       ((compose f (repeat f (- n 1)))x)))) 


(print "ex 1.43")
 (newline)
((repeat square 3) 2) 

;ex 1.44
  
 (define (smooth f) 
   (define (average a b c) (/ (+ a b c) 3))
   (let ((dx 0.000001))
     (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))
   ) 

(define (repeated f n)
  (define (iter result a)
    (if (> a 1)
       (iter (compose result f) (- a 1))
       result))
  (iter f n))
  
 (define (n-fold-smooth f n) 
   (repeated (smooth f) n)
)
 (print "ex 1.44")
 (newline)
 ((repeat square 3) 2)
((n-fold-smooth square 3) 2)
 
;ex 1. 45
(define (average x y) 
   (/ (+ x y) 2.0)) 
  
 (define (average-damp f) 
   (lambda (x) (average x (f x)))) 
  
 (define tolerance 0.00001) 
  
 (define (fixed-point f first-guess) 
   (define (close-enough? v1 v2) 
     (< (abs (- v1 v2)) tolerance)) 
   (define (try guess) 
     (let ((next (f guess))) 
       (if (close-enough? guess next) 
           next 
           (try next)))) 
   (try first-guess)) 
  
 (define (repeated f n) 
   (if (= n 1) 
       f 
       (lambda (x) (f ((repeated f (- n 1)) x))))) 
  
 (define (get-max-pow n) 
   (define (iter p r) 
     (if (< (- n r) 0) 
         (- p 1) 
         (iter (+ p 1) (* r 2)))) 
    
   (iter 1 2)) 
  
 (define (pow b p) 
   (define (even? x) 
     (= (remainder x 2) 0)) 
    
   (define (sqr x) 
     (* x x)) 
    
   (define (iter res a n) 
     (if (= n 0) 
         res 
         (if (even? n) 
             (iter res (sqr a) (/ n 2)) 
             (iter (* res a) a (- n 1))))) 
    
   (iter 1 b p)) 
  
 (define (nth-root n x) 
   (fixed-point ((repeated average-damp (get-max-pow n)) 
                 (lambda (y) (/ x (pow y (- n 1))))) 
                1.0)) 
 (print "ex 1.45")
 (newline)
(nth-root 5 32)

;ex 1.46
 (define (close-enough? v1 v2) 
   (define tolerance 1.e-6) 
   (< (/ (abs (- v1 v2)) v2)  tolerance)) 
  
 (define (iterative-improve improve close-enough?) 
   (lambda (x) 
     (let ((xim (improve x))) 
       (if (close-enough? x xim) 
           xim 
         ((iterative-improve improve close-enough?) xim)) 
       ))) 
  
 ; (a) rewrite sqrt using iterative-improve 
 (define (sqrt x) 
   ((iterative-improve 
     ; improve function is nothing but the 
     ; function f whose fixed point is to be found! 
     (lambda (y) 
       (/ (+ (/ x y) y) 2)) 
     close-enough?) 1.0)) 
  
 ; (b) rewrite fixed-point using iterative-improve 
 (define (fixed-point f first-guess) 
   ((iterative-improve 
     f 
     close-enough?) first-guess)) 
 (print "1.46")
 (newline)
  (sqrt 2)
  (fixed-point cos 1.0)