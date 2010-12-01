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
(define (make-rat n d)


