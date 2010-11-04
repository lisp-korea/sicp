;; SICP Chapter 1.2
;; everyevery, leejongsoo.club@gmail.com

;; ex 1.9.
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))
;;(define (+ a b)
;;  (if (= a 0)
;;      b
;;      (inc (+ (dec a) b)))
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; ...
;; 8
;;(define (+ a b)
;;  (if (= a 0)
;;      a
;;      (+ (dec a) (inc b))))
;; (+ 3 5)
;; (+ 2 6)
;; (+ 1 7)
;; (+ 0 8)
;; 8

;; ex 1.10.
(define (A x y)
  (display (list x y))
  (newline)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)
(define (f n) (A 0 n))
;; 2n
(define (g n) (A 1 n))
;; 2^n
(define (h n) (A 2 n))
;; n=1 => 2
;; n>1 => 2^(h(n-1)) 
(define (k n) (* 5 n n))
;; 5 * n^2