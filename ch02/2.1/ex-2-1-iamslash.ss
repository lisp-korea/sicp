;; # -*- coding: utf-8 -*-

;; ex2.1
(define (numer x)
  (car x))
(define (denom x)
  (cdr x))
(define (make-rat n d)
  (let ((g (gcd n d)))
  (cons (/ n g) (/ d g) )))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (make-rat_ n d)
  (let ((g (gcd n d)))
    (cond ((or (and (< 0 n) (< 0 d)) (and (> 0 n) (> 0 d)))
           (cons (/ (abs n) g) (/ (abs d) g) ))
          ((or (and (< 0 n) (> 0 d)) (and (> 0 n) (< 0 d)))
           (cons (- (/ (abs n) g)) (/ (abs d) g) )))))

;; ex.2.2
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment l)
  (car l))
(define (end-segment l)
  (cdr l))
(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; ex.2.3
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment l)
  (car l))
(define (end-segment l)
  (cdr l))
(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (square n)
  (* n n))

(define (make-rectangle p1 p3)
  (cons p1 p3))
(define (lt-point r)
  (car r))
(define (rb-point r)
  (cdr r))
(define (lb-point r)
  (make-point
   (x-point (lt-point r))
   (y-point (rb-point r))))
(define (rt-point r)
  (make-point
   (x-point (rb-point r))
   (y-point (lt-point r))))
(define (print-rectangle r)
  (newline)
  (display "(")
  (print-point (lt-point r))
  (print-point (lb-point r))
  (print-point (rb-point r))
  (print-point (rt-point r))
  (newline)
  (display ")")
  (newline))
(print-rectangle (make-rectangle (make-point 5 10)
                                 (make-point 10 5)))
(define (get-len p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))
(define (get-perimeter r)
  (+ (get-len (lt-point r) (lb-point r))
     (get-len (lb-point r) (rb-point r))
     (get-len (rb-point r) (rt-point r))
     (get-len (rt-point r) (lt-point r))))
(define (get-area r)
  (* (get-len (lt-point r) (rt-point r))
     (get-len (lt-point r) (lb-point r))))

(get-perimeter (make-rectangle (make-point 5 10)
                               (make-point 10 5)))
(get-area (make-rectangle (make-point 5 10)
                          (make-point 10 5)))

;; ex2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
(car (cons 1 2))
(cdr (cons 1 2))

;; ex2.5
(define (cons x y)
  (* (expt 2 x) (expt 2 y)))
(define (get-exp n x p) ;; n은 최대 x의 p승으로 나누어 떨어지는가???
  (if (= (remainder n x) 0)
      (get-exp (/ n x) x (+ p 1))
      p))
(define (car z)
  (get-exp z 2 0))
(define (cdr z)
  (get-exp z 3 0))

;; ex2.6 ???

;; ex2.7
(define (make-interval a b) (cons a b))
(define (lower-bound i)
  (car i))
(define (upper-bound i)
  (cdr i))
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
(define (print-interval i)
  (newline)
  (display "(")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display ")")
  (newline))

(print-interval (add-interval
                 (make-interval 0.1 10.1)
                 (make-interval 0.01 10.11)))
(print-interval (mul-interval
                 (make-interval 0.1 10.1)
                 (make-interval 0.01 10.11)))
(print-interval (div-interval
                 (make-interval 0.1 10.1)
                 (make-interval 0.01 10.11)))

;; ex2.8
(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (upper-bound x) (upper-bound y))))
    (if (> p1 p2)
        (make-interval p2 p1)
        (make-interval p1 p2))))
(print-interval (sub-interval
                 (make-interval 0.1 10.1)
                 (make-interval 0.01 10.01)))

;; ex2.9
(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(+ (width-interval (make-interval 0.1 10.1))
   (width-interval (make-interval 0.01 10.01)))
(width-interval (add-interval
                 (make-interval 0.1 10.1)
                 (make-interval 0.01 10.01)))
(* (width-interval (make-interval 0.1 10.1))
   (width-interval (make-interval 0.01 10.01)))
(width-interval (mul-interval
                 (make-interval 0.1 10.1)
                 (make-interval 0.01 10.01)))
(/ (width-interval (make-interval 0.1 10.1))
   (width-interval (make-interval 0.01 10.01)))
(width-interval (div-interval
                 (make-interval 0.1 10.1)
                 (make-interval 0.01 10.01)))

;; ex2.10
(define (div-interval x y)
  (if (or (= 0 (upper-bound y)) (= 0 (lower-bound y)))
      (error "ERROR: divided by zero" y)    
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
(print-interval (div-interval
                 (make-interval 0.1 10.1)
                 (make-interval 0 10.11)))
;; ex2.11 ???
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; ex2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (lower-bound i) (upper-bound i)) 2))
(define (make-center-percent c w)
  (let ((p (* c (* w 0.01))))
    (make-interval (- c p) (+ c p))))
(define (percent i)
  (let ((c (center i))
        (u (upper-bound i)))
    (* (/ (- u c) c) 100)))
(percent (make-interval 5.0 7.0))

;; ex2.13???
(define (p i1 i2)
  (percent (mul-interval i1 i2)))

;; ex2.14

;; ex2.15

;; ex2.16

