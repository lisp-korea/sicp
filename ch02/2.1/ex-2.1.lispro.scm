(define (numer x) (car x))
(define (denom x) (cdr x))
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
                      (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define (make-rat n d)
   (let ((g ((if (< d 0) - +) (gcd n d))))
     (cons (/ n g) (/ d g))))
(print "ex 2.1")
(print-rat (make-rat 3 6))
(print-rat (make-rat 3 -6))
 (newline)
 (newline)
 ;ex 2.2
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

 (define (make-point x y) (cons x y)) 
 (define (x-point p) (car p)) 
 (define (y-point p) (cdr p)) 
 (define (print-point p) 
   (newline) 
   (display "(") 
   (display (x-point p)) 
   (display ",") 
   (display (y-point p)) 
   (display ")")) 
  
 ;; Segment 
 (define (make-segment start-point end-point) 
   (cons start-point end-point)) 
 (define (start-segment segment) (car segment)) 
 (define (end-segment segment) (cdr segment)) 
  
 (define (midpoint-segment segment) 
   (define (average a b) (/ (+ a b) 2.0)) 
   (let ((a (start-segment segment)) 
         (b (end-segment segment))) 
     (make-point (average (x-point a) 
                          (x-point b)) 
                 (average (y-point a) 
                          (y-point b))))) 
  
 ;; Testing 
 (define seg (make-segment (make-point 2 3) 
                           (make-point 10 15))) 
 (print "ex 2.2")
 (print-point (midpoint-segment seg))
  (newline) 
  (newline)
 ; ex. 2.3
 ;; Point 
 (define (make-point x y) (cons x y)) 
 (define (x-point p) (car p)) 
 (define (y-point p) (cdr p)) 
  
 ;; Rectangle - 1st implementation 
  
 (define (make-rect bottom-left top-right) 
   (cons bottom-left top-right)) 
  
 ;; "Internal accessors", not to be used directly by clients.  Not sure 
 ;; how to signify this in scheme. 
 (define (bottom-left rect) (car rect)) 
 (define (bottom-right rect) 
   (make-point (x-point (cdr rect)) 
               (y-point (car rect)))) 
 (define (top-left rect) 
   (make-point (x-point (car rect)) 
               (y-point (cdr rect)))) 
 (define (top-right rect) (cdr rect)) 
  
 (define (width-rect rect) 
   (abs (- (x-point (bottom-left rect)) 
           (x-point (bottom-right rect))))) 
 (define (height-rect rect) 
   (abs (- (y-point (bottom-left rect)) 
           (y-point (top-left rect))))) 
  
 ;; Public methods. 
 (define (area-rect rect) 
   (* (width-rect rect) (height-rect rect))) 
 (define (perimeter-rect rect) 
   (* (+ (width-rect rect) (height-rect rect)) 2)) 
  
  
 ;; Usage: 
 (define r (make-rect (make-point 1 1) 
                      (make-point 3 7))) 
 (print "ex 2.3-1")
 (newline)
 (area-rect r) 
 (perimeter-rect r) 
    
 ;; --------- 
  
 ;; Alternate implementation of rectangle.  Note that this would screw 
 ;; up clients that call make-rect directly, since it uses a different 
 ;; number of args and different arg meanings, but it's generally bad 
 ;; form for clients to call constructors directly anyway, they should 
 ;; call some kind of factory method (cf "Domain Driven Design"). 
  
 ;; assuming, not checking width, height > 0. 
 (define (make-rect bottom-left width height) 
   (cons bottom-left (cons width height))) 
  
 (define (height-rect rect) (cdr (cdr rect)))  
 (define (width-rect rect) (car (cdr rect))) 
  
 ;; area and perimeter ops remain unchanged.  The internal methods from 
 ;; the first implementation won't work now. 
  
  
 ;; Usage for second implementation: 
 (define r (make-rect (make-point 1 1) 2 6)) 
 
 (print "ex 2.3-2")
 (newline)
 (area-rect r) 
 (perimeter-rect r) 
  (newline)  
 ;; Alternative Implementation II 
 ;; ----------------------------- 
 ;; 
 ;; The above implementations are limited to rectangles that have sides 
 ;; parallel to the major axes of the plane. This implementation generalizes 
 ;; to allow all rectangles. Conveniently enough, you can still use the above  
 ;; area and perimeter definitions. Abstraction barrier for the win! 
 ;; 
 ;; DO NOTE -- As above all sanity/error checking has been ignored. IRL, you 
 ;; you would want to ensure that parallel sides are actually parallel, etc. 
  
 ;; Helpful to have this 
 (define (square x) (* x x)) 
  
 ;; Point library 
 (define (make-point x y) (cons x y)) 
 (define (x-point p) (car p)) 
 (define (y-point p) (cdr p)) 
 (define (point-dist p1 p2) 
   (sqrt (+ (square (- (x-point p1) (x-point p2))) 
            (square (- (y-point p1) (y-point p2)))))) 
  
 ;; Segment library 
 (define (make-segment p1 p2) (cons p1 p2)) 
 (define (start-seg p) (car p)) 
 (define (end-seg p) (cdr p)) 
 (define (seg-len seg) (point-dist (start-seg seg) 
                                   (end-seg seg))) 
  
 ;; Rectangle library 
 (define (make-rect side parallel-side)  
   (cons side parallel-side))  
 (define (side1 rect) (car rect)) 
 (define (side2 rect) (cdr rect)) 
 (define (side-legths rect) 
   (cons (seg-len (side1 rect)) 
         (min (abs (point-dist (start-seg (side1 rect)) 
                          (start-seg (side2 rect)))) 
              (abs (point-dist (start-seg (side1 rect)) 
                          (end-seg (side2 rect))))))) 
  
 ;; Same as above 
 (define (width-rect rect) (car (side-legths rect))) 
 (define (height-rect rect) (cdr (side-legths rect))) 
  
 ;; Usage 
 (define r (make-rect (make-segment (make-point 0 1)  
                                 (make-point 0 0)) 
                   (make-segment (make-point 1 0) 
                                 (make-point 1 1))))  
  
 ;; As an alternative to this alternative, You can define you rectangles  
 ;; as a pair of perpendicular segments: 
  
 (define (make-rect side perpendicular-side)  
   (cons side perpendicular-side))  
 (define (side-legths rect) 
   (cons (seg-len (side1 rect)) 
         (seg-len (side2 rect)))) 
  
 ;ex 2.4
  ;; given: 
  
 (define (cons a b) 
   (lambda (m) (m a b))) 
  
 ;; Commentary: cons returns a function that takes a function of 2 
 ;; args, a and b.  The function will receive the values of a and b 
 ;; passed to cons when cons was called initially. 
  
 ;; z is a function that takes a 2-arg function.  That inner function 
 ;; will be passed p and q in that order, so just return the first arg, p. 
 (define (car z) 
   (z (lambda (p q) p))) 
  
  
 ;; ... so this is obvious. 
 (define (cdr z) 
   (z (lambda (p q) q))) 
  
  (print "ex 2.4")
  (newline)
  
 ;; Usage: 
 (define x (cons 3 4)) 
 (car x) 
 (cdr x) 
  (newline)
 ;ex 2.5
   (define (exp base n) 
   (define (iter x result) 
     ;; invariant: base^x * result is constant. 
     (if (= 0 x) 
         result 
         (iter (- x 1) (* base result)))) 
   (iter n 1)) 
  
  
 (define (count-0-remainder-divisions n divisor) 
   (define (iter try-exp) 
     (if (= 0 (remainder n (exp divisor try-exp))) 
         (iter (+ try-exp 1))  ;; Try another division. 
         (- try-exp 1))) 
  
   ;; We don't need to try 0 divisions, as that will obviously pass. 
   (iter 1)) 
  
  
 ;; cons, car, cdr 
 (define (my-cons a b) (* (exp 2 a) (exp 3 b))) 
 (define (my-car z) (count-0-remainder-divisions z 2)) 
 (define (my-cdr z) (count-0-remainder-divisions z 3)) 
  
(print "ex 2.5")
(newline)
 ;; Usage: 
 (define test (my-cons 11 17)) 
 (my-car test) 
 (my-cdr test) 
(newline)

;ex 2.6
 (define one (lambda (f) (lambda (x) (f x)))) 
 (define two (lambda (f) (lambda (x) (f (f x))))) 
 (define (add-1 a b) 
   (lambda (f) 
     (lambda (x) 
       ((a f) ((b f) x))))) 

 (print "ex2.6")
 (newline)
 ((one square) 2)
 ((two square) 2)
(((add-1 two one) square) 2)