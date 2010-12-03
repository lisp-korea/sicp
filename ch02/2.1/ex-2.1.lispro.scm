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
 (newline)
;ex 2.7
 (define (make-interval a b) (cons a b)) 
 (define (upper-bound interval) (min (car interval) (cdr interval))) 
 (define (lower-bound interval) (max (car interval) (cdr interval))) 
(print "ex 2.7")
 (newline)
 ;; Usage 
 (define i (make-interval 2 7)) 
 (upper-bound i) 
 (lower-bound i) 
  
 (define j (make-interval 8 3)) 
 (upper-bound j) 
 (lower-bound j) 
 (newline)
 ;ex 2.8
 ;; The max and min can be supplied to the constructor in any order. 
 (define (make-interval a b) (cons a b)) 
 (define (upper-bound interval) (max (car interval) (cdr interval))) 
 (define (lower-bound interval) (min (car interval) (cdr interval))) 
  
 ;; The minimum value would be the smallest possible value 
 ;; of the first minus the largest of the second.  The maximum would be 
 ;; the largest of the first minus the smallest of the second. 
 (define (sub-interval x y) 
   (make-interval (- (lower-bound x) (upper-bound y)) 
                  (- (upper-bound x) (lower-bound y)))) 
  
 (define (display-interval i) 
   (newline) 
   (display "[") 
   (display (lower-bound i)) 
   (display ",") 
   (display (upper-bound i)) 
   (display "]")) 
(print "ex 2.8->")
(print "[a - c, b - d]")
 ;; Usage 
 (define i (make-interval 2 7)) 
 (define j (make-interval 8 3)) 
  
 (display-interval i) 
 (display-interval (sub-interval i j)) 
 (display-interval (sub-interval j i)) 
(newline)
(newline)
 ;ex 2.9
(print "ex 2.9")
(newline)
(print "[0, 10] * [0, 2] = [0, 20]   (width = 10)")
(newline)
(print "0-sub operation")
(newline)
(print "[-5, 5] * [0, 2] = [0, 10]   (width = 5)")
(newline)
(print "0-product operation")
(newline)
(newline)

;ex 2.10
(define (mul-interval x y) 
   (let ((p1 (* (lower-bound x) (lower-bound y))) 
         (p2 (* (lower-bound x) (upper-bound y))) 
         (p3 (* (upper-bound x) (lower-bound y))) 
         (p4 (* (upper-bound x) (upper-bound y)))) 
     (make-interval (min p1 p2 p3 p4) 
                    (max p1 p2 p3 p4)))) 
  
 (define (div-interval x y) 
   (mul-interval x  
                 (make-interval (/ 1. (upper-bound y)) 
                                (/ 1. (lower-bound y))))) 
  
 (define (print-interval name i) 
   (newline) 
   (display name) 
   (display ": [") 
   (display (lower-bound i)) 
   (display ",") 
   (display (upper-bound i)) 
   (display "]")) 
  
 (print "ex 2.10")
 (newline)
 ;; Usage 
 (define i (make-interval 2 7)) 
 (define j (make-interval 8 3)) 
  
 (print-interval "i" i) 
 (print-interval "j" j) 
 (print-interval "i*j" (mul-interval i j)) 
 (print-interval "j*i" (mul-interval j i)) 
 (print-interval "i/j" (div-interval i j)) 
 (print-interval "j/i" (div-interval j i)) 
  
 ;; Gives: 
 ;; i: [2,7] 
 ;; j: [3,8] 
 ;; i*j: [6,56] 
 ;; j*i: [6,56] 
 ;; i/j: [.25,2.333333333333333] 
 ;; j/i: [.42857142857142855,4.] 
  
  
 ;; New definition: Division by interval spanning 0 should fail. 
 (define (div-interval x y) 
   (if (>= 0 (* (lower-bound y) (upper-bound y))) 
       (error "Division error (interval spans 0)" y) 
       (mul-interval x  
                     (make-interval (/ 1. (upper-bound y)) 
                                    (/ 1. (lower-bound y)))))) 
  
 (define span-0 (make-interval -1 1)) 
 (print-interval "i/j" (div-interval i j)) 
;(print-interval "i/span-0" (div-interval i span-0))  ;;;; error test
  
 ;; Results: 
 ;; i/j: [.25,2.333333333333333] 
 ;; ;Division error (interval spans 0) (-1 . 1) 
(newline)

;ex 2.11

;; Old multiplication (given) 
 (define (old-mul-interval x y) 
   (let ((p1 (* (lower-bound x) (lower-bound y))) 
         (p2 (* (lower-bound x) (upper-bound y))) 
         (p3 (* (upper-bound x) (lower-bound y))) 
         (p4 (* (upper-bound x) (upper-bound y)))) 
     (make-interval (min p1 p2 p3 p4) 
                    (max p1 p2 p3 p4)))) 
  
  
 ;; This looks a *lot* more complicated to me, and with the extra 
 ;; function calls I'm not sure that the complexity is worth it. 
 (define (mul-interval x y) 
   ;; endpoint-sign returns: 
   ;;     +1 if both endpoints non-negative, 
   ;;     -1 if both negative, 
   ;;      0 if opposite sign 
   (define (endpoint-sign i) 
     (cond ((and (>= (upper-bound i) 0) 
                 (>= (lower-bound i) 0)) 
            1) 
           ((and (< (upper-bound i) 0) 
                 (< (lower-bound i) 0)) 
            -1) 
           (else 0))) 
  
   (let ((es-x (endpoint-sign x)) 
         (es-y (endpoint-sign y)) 
         (x-up (upper-bound x)) 
         (x-lo (lower-bound x)) 
         (y-up (upper-bound y)) 
         (y-lo (lower-bound y))) 
  
     (cond ((> es-x 0) ;; both x endpoints are +ve or 0 
            (cond ((> es-y 0) 
                   (make-interval (* x-lo y-lo) (* x-up y-up))) 
                  ((< es-y 0) 
                   (make-interval (* x-up y-lo) (* x-lo y-up))) 
                  (else 
                   (make-interval (* x-up y-lo) (* x-up y-up))))) 
  
           ((< es-x 0) ;; both x endpoints are -ve 
            (cond ((> es-y 0) 
                   (make-interval (* x-lo y-up) (* x-up y-lo))) 
                  ((< es-y 0) 
                   (make-interval (* x-up y-up) (* x-lo y-lo))) 
                  (else 
                   (make-interval (* x-lo y-up) (* x-up y-lo))))) 
  
           (else  ;; x spans 0 
            (cond ((> es-y 0) 
                   (make-interval (* x-lo y-up) (* x-up y-up))) 
                  ((< es-y 0) 
                   (make-interval (* x-up y-lo) (* x-lo y-lo))) 
                  (else 
                   ;; Both x and y span 0 ... need to check values 
                   (make-interval (min (* x-lo y-up) (* x-up y-lo)) 
                                  (max (* x-lo y-lo) (* x-up y-up))))))))) 
 (define (eql-interval? a b) 
   (and (= (upper-bound a) (upper-bound b)) 
        (= (lower-bound a) (lower-bound b)))) 
  
 ;; Fails if the new mult doesn't return the same answer as the old 
 ;; naive mult. 
 (define (ensure-mult-works aH aL bH bL) 
   (let ((a (make-interval aL aH)) 
         (b (make-interval bL bH))) 
   (if (eql-interval? (old-mul-interval a b) 
                      (mul-interval a b)) 
       true 
       (error "new mult returns different value!"  
              a  
              b  
              (old-mul-interval a b) 
              (mul-interval a b))))) 
  
  
 ;; The following is overkill, but it found some errors in my 
 ;; work.  The first two #s are the endpoints of one interval, the last 
 ;; two are the other's.  There are 3 possible layouts (both pos, both 
 ;; neg, one pos one neg), with 0's added for edge cases (pos-0, 0-0, 
 ;; 0-neg). 
  (newline)
  (print "ex 2.11")
  (newline)
 (ensure-mult-works  +10 +10   +10 +10) 
 (ensure-mult-works  +10 +10   +00 +10) 
 (ensure-mult-works  +10 +10   +00 +00) 
 (ensure-mult-works  +10 +10   +10 -10) 
 (ensure-mult-works  +10 +10   -10 +00) 
 (ensure-mult-works  +10 +10   -10 -10) 
  
 (ensure-mult-works  +00 +10   +10 +10) 
 (ensure-mult-works  +00 +10   +00 +10) 
 (ensure-mult-works  +00 +10   +00 +00) 
 (ensure-mult-works  +00 +10   +10 -10) 
 (ensure-mult-works  +00 +10   -10 +00) 
 (ensure-mult-works  +00 +10   -10 -10) 
  
 (ensure-mult-works  +00 +00   +10 +10) 
 (ensure-mult-works  +00 +00   +00 +10) 
 (ensure-mult-works  +00 +00   +00 +00) 
 (ensure-mult-works  +00 +00   +10 -10) 
 (ensure-mult-works  +00 +00   -10 +00) 
 (ensure-mult-works  +00 +00   -10 -10) 
  
 (ensure-mult-works  +10 -10   +10 +10) 
 (ensure-mult-works  +10 -10   +00 +10) 
 (ensure-mult-works  +10 -10   +00 +00) 
 (ensure-mult-works  +10 -10   +10 -10) 
 (ensure-mult-works  +10 -10   -10 +00) 
 (ensure-mult-works  +10 -10   -10 -10) 
  
 (ensure-mult-works  -10 +00   +10 +10) 
 (ensure-mult-works  -10 +00   +00 +10) 
 (ensure-mult-works  -10 +00   +00 +00) 
 (ensure-mult-works  -10 +00   +10 -10) 
 (ensure-mult-works  -10 +00   -10 +00) 
 (ensure-mult-works  -10 +00   -10 -10) 
  
 (ensure-mult-works  -10 -10   +10 +10) 
 (ensure-mult-works  -10 -10   +00 +10) 
 (ensure-mult-works  -10 -10   +00 +00) 
 (ensure-mult-works  -10 -10   +10 -10) 
 (ensure-mult-works  -10 -10   -10 +00) 
 (ensure-mult-works  -10 -10   -10 -10) 

 ;ex 2.12
 (define (center i) (/ (+ (upper-bound i) (lower-bound i)) 2)) 
  
 ;; Percent is between 0 and 100.0 
 (define (make-interval-center-percent c pct) 
   (let ((width (* c (/ pct 100.0)))) 
     (make-interval (- c width) (+ c width)))) 
  
 (define (percent-tolerance i) 
   (let ((center (/ (+ (upper-bound i) (lower-bound i)) 2.0)) 
         (width (/ (- (upper-bound i) (lower-bound i)) 2.0))) 
     (* (/ width center) 100))) 
  
  
 ;; A quick check: 
  (newline)
  (print "ex 2.12")
  (newline)
 (define i (make-interval-center-percent 10 50)) 
 (lower-bound i) 
 (upper-bound i) 
 (center i) 
 (percent-tolerance i)
 
 (newline)
 
 ;ex 2.13
 ;a = [Ca*(1 - 0.5*Ta), Ca*(1 + 0.5*Ta)]
; b = [Cb*(1 - 0.5*Tb), Cb*(1 + 0.5*Tb)]
; a*b = [Ca*Cb*(1 - 0.5*(Ta + Tb) + 0.25*Ta*Tb),
;         Ca*Cb*(1 + 0.5*(Ta + Tb) + 0.25*Ta*Tb)]
(print "ex 2.13")
(newline)
;; Percent is between 0 and 100.0 
 (define (make-interval-center-percent c pct) 
   (let ((width (* c (/ pct 100.0)))) 
     (make-interval (- c width) (+ c width)))) 
  
 (define (percent-tolerance i) 
   (let ((center (/ (+ (upper-bound i) (lower-bound i)) 2.0)) 
         (width (/ (- (upper-bound i) (lower-bound i)) 2.0))) 
     (* (/ width center) 100))) 
  
 (define (mul-interval x y) 
   (let ((p1 (* (lower-bound x) (lower-bound y))) 
         (p2 (* (lower-bound x) (upper-bound y))) 
         (p3 (* (upper-bound x) (lower-bound y))) 
         (p4 (* (upper-bound x) (upper-bound y)))) 
     (make-interval (min p1 p2 p3 p4) 
                    (max p1 p2 p3 p4)))) 
  
  
 (define i (make-interval-center-percent 10 0.5)) 
 (define j (make-interval-center-percent 10 0.4)) 
 (percent-tolerance (mul-interval i j)) 
  
(newline)

;ex 2.14
