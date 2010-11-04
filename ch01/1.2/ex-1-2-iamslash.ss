# -*- coding: utf-8 -*-

(require (lib "trace.ss"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.9
(define (dec x)
  (- x 1))

(define (inc x)
  (+ x 1))

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; linear recursive process이다.

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; linear iterative process이다.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10) ;; 1024
(A 2 4)  ;; 65536
(A 3 3)  ;; 65536

(define (f n) (A 0 n))   ;; 2 * n
(define (g n) (A 1 n))   ;; 2 ^ n
;; n == 1일때 (A x 1) 은 2이다.
;;
;; n >= 2일때 
;; (A 0 (A 1 (- n 1)))
;; (A 0 (A 0 (A 1 (- (- n 1) 1))))
;; (A 0 (A 0 (A 0 (A 1 (- (- (- n 1) 1) 1)))))
;; (A 0 (A 0 (A 0 ... (A 1 1) ...)))
;; (A 0 (A 0 (A 0 ... 2 ...)))
;; 2 ^ n

(define (h n) (A 2 n))
;; (h n) = 2     (n == 1)
;; (h n) = 2 ^ n (n >= 2)

;; n == 1일때 (A x 1)은 2이다.
;;
;; n >= 2일때
;; (A 1 (A 2 (- n 1)))
;; (A 1 (A 1 (- (- n 1) 1)))
;; (A 1 (A 1 (A 1 ... (A 2 1) ...)))
;; (A 1 (A 1 (A 1 ... 2 ... )))
;; 2 ^ 2 ^ 2 ...

(define (k n) (* 5 n n)) ;; 5 * n ^ 2

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.10
;;
;; WARNING) 오타 !!!
;;
;; Exercise 1.11.  A function f is defined by the rule that f(n) = n
;; if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a
;; procedure that computes f by means of a recursive process. Write a
;; procedure that computes f by means of an iterative process.

;; linear recursive procedure
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

;; iterative recursive procedure
;; a : f(n-1)
;; b : f(n-2)
;; c : f(n-3)
(define (f n)
  (f-iter 2 1 0 n))
(define (f-iter a b c count)
  (cond ((= count 0) 0)
        ((= count 1) 1)
        ((= count 2) a)
        (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.11 ???

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.12 ???

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.13
