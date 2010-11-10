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

;; ex 1.11.
(define (func-f-recur n)
  (cond
   ((< n 3) n)
   (else (+
	  (func-f-recur (- n 1))
	  (* 2 (func-f-recur (- n 2)) )
	  (* 3 (func-f-recur (- n 3)))))))
;; (func-f-recur 4)
(define (func-f-iter n)
  (define (func-f-iter-r a b c d n)
    (cond
     ((= a n) (+ b (* 2 c) (* 3 d)))
     (else (func-f-iter-r (+ a 1) (+ b (* 2 c) (* 3 d)) b c n))))
  (cond
   ((< n 3) n)
   (else (func-f-iter-r 3 2 1 0 n))))
;;(func-f-iter 4)

;; ex 1.12.
(define (pascal-num x y)
  (cond
   ((= x 1) 1)
   ((= x y) 1)
   (else (+ (pascal-num (- x 1) (- y 1)) (pascal-num x (- y 1))))))
;;(pascal-num 1 1)
;;(pascal-num 1 2)
;;(pascal-num 2 2)
;;(pascal-num 1 3)
;;(pascal-num 2 3)
;;(pascal-num 3 3)
(define (pascal-num-row i n)
  (cond
   ((= i n) (cons (pascal-num i n) '()))
   (else (cons (pascal-num i n) (pascal-num-row (+ i 1) n)))))
(define (pascal-triangle n)
  (pascal-num-row 1 n))
;; (pascal-triangle 6)

;; ex 1.13.
;; skip