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

