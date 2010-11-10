;; ex-1.1
10					;10

(+ 5 3 4)				;12

(- 9 1)					;8

(/ 6 2)					;3

(+ (* 2 4) (- 4 6))			;6

(define a 3)				;a

(define b (+ a 1))			;b

(+ a b (* a b))				;19

(= a b)					;#f

(if (and (> b a) (< b (* a b)))
    b
    a)					;4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))			;16

(+ 2 (if (> b a) b a))			;6

(* (cond ((> a b) a)
	 ((< a b) b)
	 (else - 1))
   (+ a 1))				;16


;; ex-1.2
(/ (+ 5
      4
      (- 2
	 (- 3
	    (+ 6
	       (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))		;-37/150


;; ex-1.3
(define (square x) (* x x))

(define (calc a b c)
  (cond ((and (<= a b) (<= a c))
	 (+ (square b) (square c)))
	((and (<= b a) (<= b c))
	 (+ (square a) (square c)))
	(else
	 (+ (square a) (square b)))))


;; ex-1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


;; ex-1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))


;; ex-1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else-clause)))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

;; (define (sqrt-iter guess x)
;;   (if (good-enough? guess x)
;;       guess
;;       (sqrt-iter (improve guess x)
;; 		 x)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))


;; ex-1.7
(sqrt 0.0001)				;.03230844833048122

(sqrt 9999999999999999)			;100000000.

(define (better? new-guess guess)
  (> (abs (- new-guess guess))
     (* new-guess 0.001)))

(define (sqrt-iter guess x)
  (define new-guess (improve guess x))
  (if (better? new-guess guess)
      (sqrt-iter new-guess x)
      guess))


;; ex-1.8
(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x)
		      x)))

(define (good-enough? guess x)
    (< (abs (- (cube guess) x))
       0.001))

(define (cube x)
  (* x x x))

(define (improve guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))
