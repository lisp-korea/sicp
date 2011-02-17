(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define x (cons 1 2))
(car x)
(cdr x)
; 2

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
; 1

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mult-rat x y)
  (make-rat (* (car x) (car y))
            (* (cdr x) (cdr y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))

(print-rat (mult-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))  

;; ex 2.1

(define (deter-sign n d)
  (cond ((< 0 (* n d)) 
         (if (< n 0) 
             (make-rat (- n) (- d))
             (make-rat n d)))
        (else (if (> n 0) (make-rat (- n) (- d))
                  (make-rat n d)))))

(print-rat (deter-sign -1 -2))
;1/2> 
(print-rat (deter-sign -1 -2))
;1/2>
(print-rat (deter-sign 1 -2))
;-1/2>


;; ex 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-point x y)
  (cons x y))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point (/ (+ (car (start-segment segment)) (car (end-segment segment))) 2)
              (/ (+ (cdr (start-segment segment)) (cdr (end-segment segment))) 2)))

 (print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 9 10))))
; (5,6)

;; ex 2.3
; second
(define (make-point x y)
  (cons x y))

(define (make-size w h)
  (cons w h))

(define (make-rectangle p s)
  (cons p s))

(define (size-of-rectangle rect)
  (let ((size (cdr rect)))
    (* (car size) (cdr size))))

(define (boundary-of-rectangle rect)
  (let ((size (cdr rect)))
    (* (+ (car size) (cdr size)) 2)))

(define (print-rectangle rect)
  (newline)
  (display "(size:")
  (display (size-of-rectangle rect))
  (display ",boundary:")
  (display (boundary-of-rectangle rect))
  (display ")"))

;; 2.1.3 
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

; if (cons x y) -> z
; (car (cons x y) 0)

;; ex 2.4
(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(define (my-car z)
  (z (lambda (p q) p)))

((lambda (m) (m 1 2)) (lambda (p q) p))
((lambda (p q) p) 1 2)
;; 1

;; ex 2.5
;; x = 2^a * 3^b
;; log x = log2(2^a*3^b)
;; log2(2^a) + log2(3^b)
;; log x = a + b*log2(3)

(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (devide? n d)
  (if (= 0 (remainder n d)) #t
      #f))

(define (find-exp x e i)
  (if (devide? x e) (find-exp (/ x e) e (+ i 1))
      i))

(define (my-car x)
  (find-exp x 2 0))

(define (my-cdr x)
  (find-exp x 3 0))
        

;; ex 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (add-1 zero)
;==>
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f x)))

(define two
  (add-1 one)
;==>
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (f x))))



;; text 2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
;; ex 2.7
;(define (make-interval a b) (cons a b))

(define (make-interval lower upper) (cons lower upper))
(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

(define x (make-interval -1.5 1.5))
(define y (make-interval 0.0 10.0))
(add-interval x y)


;; ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(sub-interval x y)

;; ex 2.9
(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(interval-width x)

(= (interval-width (add-interval x y))
   (+ (interval-width x) (interval-width y)))

(= (interval-width (mul-interval x y))
   (* (interval-width x) (interval-width y)))

;; ex 2.10
(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "devide by interval containing zero!")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
(div-interval x y)
(div-interval y x)

;; ex 2.11
;; (define (mul-interval x y)
;;   (let ((p1 (* (lower-bound x) (lower-bound y)))
;;         (p2 (* (lower-bound x) (upper-bound y)))
;;         (p3 (* (upper-bound x) (lower-bound y)))
;;         (p4 (* (upper-bound x) (upper-bound y))))
;;     (make-interval (min p1 p2 p3 p4)
;;                    (max p1 p2 p3 p4))))
;;
;;     x  y
;; 1. ++ ++
;; 2. ++ -+
;; 3. ++ --
;; 4. -+ ++
;; 5. -+ -+
;; 6. -+ --
;; 7. -- ++
;; 8. -- -+
;; 9. -- --

(define (mul-interval)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond 
     ;; 1. ++ ++
     ((and (>= lx 0) (>= ux 0) (>= ly 0) (>= uy 0))
      (make-interval (* lx ly) (* ux uy)))
     ;; 2. ++ -+
     ((and (>= lx 0) (>= ux 0) (< ly 0) (>= uy 0))
      (make-interval (* lx ly) (* ux uy)))
     ;; 3. ++ --
     ((and (>= lx 0) (>= ux 0) (< ly 0) (< uy 0))
      (make-interval (* ux ly) (* lx uy)))
     ;; 4. -+ ++
     ((and (>= lx 0) (>= ux 0) (< ly 0) (>= uy 0))
      (make-interval (* lx ly) (* ux uy)))
     ;; 5. -+ -+
     ((and (< lx 0) (>= ux 0) (< ly 0) (>= uy 0))
      (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy))))
     ;; 6. -+ --
     ((and (< lx 0) (>= ux 0) (< ly 0) (< uy 0))
      (make-interval (* ux uy) (* lx uy)))
     ;; 7. -- ++
     ((and (< lx 0) (< ux 0) (>= ly 0) (>= uy 0))
      (make-interval (* ux ly) (* lx uy)))
     ;; 8. -- -+
     ((and (< lx 0) (< ux 0) (< ly 0) (>= uy 0))
      (make-interval (* lx uy) (* ux ly)))
     ;; 9. -- --
     ((and (< lx 0) (< ux 0) (< ly 0) (< uy 0))
      (make-interval (* ux uy) (* lx ly)))
     )))
     
;; text
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i ) (lower-bound i)) 2))

;; ex 2.12
(define (make-center-percent c p)
  (let ((width (/ (* c p) 100)))
    (make-center-width c width)))

(define (percent x)
  (* 100 (/ (width x) (center x))))

(define z (make-center-percent 3.5 10))

;; ex 2.13
;; Assume that all numbers are plus.
(define x (make-center-percent 10 0.00000001))
(define y (make-center-percent 50 0.00000001))

(define dz1 (percent (mul-interval x y)))

;; z + dz  = (x + dx) * (y + dy)
;; z + dz = xy + xdy + ydx + dxdy
;; dx << x, dy << y
;; z + dz = xy + xdy + ydx
;; dz = xdy + ydx

(define dz (+ (* (center x) (percent y)) (* (center x) (percent y))))

;; text

(define r1 (make-center-percent 10 5))
(define r2 (make-center-percent 5 1))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1 )))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; 2.14
(define r1 (make-center-percent 10 0.005))
(define r2 (make-center-percent 5 0.00001))

(define rAA (div-interval r1 r1))
(define rAB (div-interval r1 r2))

;;  R1R2     (R1+d1) (R2+d2)
;; ------  = ----------------
;; R1 + R2  (R1+d1) + (R2+d2)

;;     1                1           (R1+d1) (R2+d2)
;;----------   = -------------- != ----------------
;;  1     1         1       1      (R2+d2) + (R1+d1)
;;---- + ----     ----- + -----
;; R1     R2      R1+d1   R2+d2
;;
;; Of course not. After all, it’s just two intervals divided one by another, and all the rules apply. No wonder Lem is getting different answers – he is ;; using two different formulas!


;; 2.15
;; interval arithmetic system
(define a (make-interval 2 4))
(define b (make-interval -2 0))
(define c (make-interval 3 8))

;; x = a(b + c), y = ab + ac
(define x (mul-interval a (add-interval b c)))
(define y (add-interval (mul-interval a b) (mul-interval a c)))

;; Solution 1
;;This is a combined answer to both exercises:

;;Eva Lu Ator is right, and an example can be seen in the computation done in the previous exercise – par2 produces tighter bounds than par1. To understand why this is so, I will try to explain the problems with interval arithmetic (answer to 2.16):

;; In doing arithmetic, we rely on some laws to hold without giving them much thought. Speaking mathematically, the real numbers are fields. For example, we expect to have an inverse element for addition – for each element A to have an element A’ so that A + A’ = 0. It is easy to check that this doesn’t hold for intervals! An inverse element for multiplication also doesn’t exist (this is the problem we saw in exercise 2.14). The distributive law doesn’t hold – consider the expression [1,2] * ([-3,-2] + [3,4]) – it makes a difference whether you do the additions or the multiplication first.

;; To solve these problems at least for the simple arithmetic package we’re developing, I think we need to define the concept of identity for an interval. Operations must be able to identify if two operators are identical, and adjust the results accordingly.

;; Soluation 2
;; 2.15
;; Eva is right. Program par2 is better than program par1. As we have observed in SICP Exercise 2.14, Alyssa’s system will produce big errors when computing expressions such as A/A. The reason is the following. When an interval A appears twice in an expression, they should not behave independently. They are correlated. In fact, they are the same unit in a system. So if the real value of A is , both A‘s should have the same value in a good system. However, Alyssa’s program treats them independently. In Alyssa’s system, one A may have a value of , whereas the other A may have a value of . Therefore we conclude that Alyssa’s system is flawed.

;; 2.16
;; We have observed in SICP Exercise 2.14 and 2.15 that equivalent algebraic expressions may lead to different answers. I think it is because an interval that appears multiple times in an expression will behave independently though they should behave exactly the same way.

;;Can we devise an interval-arithmetic package that does not have this shortcoming? The authors of SICP warned us of the difficulty of this problem. But talk is cheap. So I will still try to say something.

;;We should take Eva Lu Ator’s suggestion (SICP Exercise 2.15) to avoid repeated appearance of intervals.
;;Remember the Taylor expansion: . Maybe we could design a system which utilizes the Taylor expansion, assuming that percentage tolerances are small. To compute the resulting center of an expression, we perform normal arithmetics on the centers of argument intervals. Then we compute the partial derivatives by varying the value of one interval while keeping the others fixed. Finally we combine the partial derivatives and percentage tolerances together to obtain the resulting percentage tolerance.
;;For really complicated systems, we could also perform Monte carlo simulations to get an answer that is good enough.
