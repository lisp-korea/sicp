
;; chapter 2.1 데이터 요약

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom y))))

(define (make-rat n d)
  (cons n d))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (display "\n"))

(define (make-rat n d)
  (define (gcd a b)
    (if (= b 0)
	a
	(gcd b (remainder a b))))
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; 연습문제 2.1

(define (make-rat n d)
  (define (gcd a b)
    (if (= b 0)
	a
	(gcd b (remainder a b))))
  (let ((g  (gcd (if (> n 0) n (- n)) d)))
    (cons (/ n g) (/ d g))))

(+ 1/2 -1/3)
;=> 1/6
(print-rat (add-rat (make-rat 1 2) (make-rat -1 3)))
;=> 1/6


;; 연습문제 2.2

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (define (average a b)
    (* 0.5 (+ a b)))
  (let ((start-seg (start-segment segment))
	(end-seg (end-segment segment)))
    (make-point (average (x-point start-seg)
			 (x-point end-seg))
		(average (y-point start-seg)
			 (y-point end-seg)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 10 10))))
;=>(5.0, 5.0)


;; 연습문제 2.3

(define (make-rectangle p1 p2)
  (make-segment p1 p2))

(define (circumference rectangle)
  (let ((start-seg (start-segment rectangle))
	(end-seg (end-segment rectangle)))
    (+ (* 2 (abs (- (x-point start-seg)
		    (x-point end-seg))))
       (* 2 (abs (- (y-point start-seg)
		    (y-point end-seg)))))))

(circumference (make-rectangle (make-point 0 0) (make-point 8 7)))
;=> 30

(define (area rectangle)
  (let ((start-seg (start-segment rectangle))
	(end-seg (end-segment rectangle)))
    (* (abs (- (x-point start-seg)
	       (x-point end-seg)))
       (abs (- (y-point start-seg)
	       (y-point end-seg))))))

(area (make-rectangle (make-point 8 7) (make-point 0 0)))
;=> 56


;; 연습문제 2.4

(define (si-cons x y)
  (lambda (m) (m x y)))

(define (si-car z)
  (z (lambda (p q) p)))

(si-car (si-cons 100 10))
;=> 100


(define (si-cdr z)
  (z (lambda (p q) q)))

(si-cdr (si-cons 100 10))
;=> 10

;; (si-cdr (si-cons 100 10))
;; (si-cdr (lambda (m) (m 100 10)))
;; ((lambda (m) (m 100 10)) (lambda (p q) q))
;; ((lambda (p q) q) 100 10)
;; 10


;; 연습문제 2.5



;; 연습문제 2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f (((lambda (n) (lambda (y) y)) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (y) y) x))))
;; (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

;; (add-1 one)
;; (lambda (f) (lambda (x) (f ((one f) x))))
;; (lambda (f) (lambda (x) (f (((lambda (n) (lambda (y) (n y))) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (y) (f y)) x))))
;; (lambda (f) (lambda (x) (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))
  


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-inerval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y)))))


;; 연습문제 2.7

(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (car interval))

(define (lower-bound interval)
  (cdr interval))


;; 연습문제 2.8

(define (sub-interval x y)
  (make-ineterval (- (lower-bound x) (upper-bound y))
		  (- (upper-bound x) (lower-bound y))))


;; 연습문제 2.10
(define (bound-zero? x)
  (or (> (lower-bound x) 0)
      (> 0 (upper-bound x))))


(define (div-interval x y)
  (if (bound-zero? y)
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				     (/ 1.0 (lower-bound y))))
      (display "ERROR!!!!")))

;; 연습문제 2.11

