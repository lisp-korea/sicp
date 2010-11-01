;; SICP Chapter 1.1

;; note
;; substitution model
;; applicative order versus normal order

;; ex 1.1.
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= 3 4) b)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> b a) b a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))

;; ex 1.2.
;; skip

;; ex 1.3.
(define (sum-of-squares-of-two-larger-nums a b c)
  (if
   (>= a b)
   (if (>= b c)
       (+ a b)
       (+ a c)
       )
   (if (>= a c)
       (+ b a)
       (+ b c))))
(sum-of-squares-of-two-larger-nums 11 12 10)

;; ex 1.4.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 1 -2)

;; ex 1.5.
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;;(test 0 (p))

;; note
;; substitution model
;; applicative order versus normal order
;; SICP Chapter 1.1

;; ex 1.1.
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= 3 4) b)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> b a) b a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))

;; ex 1.2.
;; skip

;; ex 1.3.
(define (sum-of-squares-of-two-larger-nums a b c)
  (if
   (>= a b)
   (if (>= b c)
       (+ a b)
       (+ a c)
       )
   (if (>= a c)
       (+ b a)
       (+ b c))))
(sum-of-squares-of-two-larger-nums 11 12 10)

;; ex 1.4.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 1 -2)

;; ex 1.5.
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;;(test 0 (p))

;; note
;; substitution model
;; applicative order versus normal order
;; ex 1.6.


;; SICP Chapter 1.1

;; ex 1.1.
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= 3 4) b)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> b a) b a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))

;; ex 1.2.
;; skip

;; ex 1.3.
(define (sum-of-squares-of-two-larger-nums a b c)
  (if
   (>= a b)
   (if (>= b c)
       (+ a b)
       (+ a c)
       )
   (if (>= a c)
       (+ b a)
       (+ b c))))
(sum-of-squares-of-two-larger-nums 11 12 10)

;; ex 1.4.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 1 -2)

;; ex 1.5.
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;;(test 0 (p))

;; note
;; substitution model
;; applicative order versus normal order
;; SICP Chapter 1.1

;; ex 1.1.
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= 3 4) b)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> b a) b a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))

;; ex 1.2.
;; skip

;; ex 1.3.
(define (sum-of-squares-of-two-larger-nums a b c)
  (if
   (>= a b)
   (if (>= b c)
       (+ a b)
       (+ a c)
       )
   (if (>= a c)
       (+ b a)
       (+ b c))))
(sum-of-squares-of-two-larger-nums 11 12 10)

;; ex 1.4.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 1 -2)

;; ex 1.5.
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;;(test 0 (p))

;; ex 1.6.
(define (average a b)
  (/ (+ a b) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (display guess)
  (newline)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt-new x) (sqrt-iter 1.0 x))

;;(sqrt-new 100.0)
;;(sqrt 100.0)

;; ex 1.6.