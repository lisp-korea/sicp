
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
