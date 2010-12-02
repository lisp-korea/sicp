(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define x (cons 1 2))
(car x)
(cdr x)
; 2

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
; 1

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (car x) (cdr y))
               (* (car y) (cdr x)))
            (* (cdr x) (cdr y))))

(define (mult-rat x y)
  (make-rat (* (car x) (car y))
            (* (cdr x) (cdr y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))

(print-rat (mult-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))  

;; ex 2.1

(define (deter-sign n d)
  (cond ((< 0 (* n d)) (if (< n 0) (make-rat (- n) (- d))
                           (make-rat n d)))
        (else (if (> n 0) (make-rat (- n) (- d))
                  (make-rat n d)))))

(print-rat (deter-sign -1 -2))
;1/2> 
(print-rat (deter-sign -1 -2))
;1/2>
(print-rat (deter-sign 1 -2))
;-1/2>


;; ex 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-point x y)
  (cons x y))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point (/ (+ (car (start-segment segment)) (car (end-segment segment))) 2)
              (/ (+ (cdr (start-segment segment)) (cdr (end-segment segment))) 2)))

 (print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 9 10))))
; (5,6)

;; ex 2.3
; second
(define (make-point x y)
  (cons x y))

(define (make-size w h)
  (cons w h))

(define (make-rectangle p s)
  (cons p s))

(define (size-of-rectangle rect)
  (let ((size (cdr rect)))
    (* (car size) (cdr size))))

(define (boundary-of-rectangle rect)
  (let ((size (cdr rect)))
    (* (+ (car size) (cdr size)) 2)))

(define (print-rectangle rect)
  (newline)
  (display "(size:")
  (display (size-of-rectangle rect))
  (display ",boundary:")
  (display (boundary-of-rectangle rect))
  (display ")"))

;; 2.1.3 
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

; if (cons x y) -> z
; (car (cons x y) 0)

;; ex 2.4
(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(define (my-car z)
  (z (lambda (p q) p)))

((lambda (m) (m 1 2)) (lambda (p q) p))
;; 1

;; ex 2.5



































(defun divides? (a b)
  (= (rem b a) 0))

(defun my-cons (a b)
  (* (expt 2 a) (expt 3 b)))

(defun my-car (z)
  (do ( (n 0 (1+ n))
        (aa z (/ aa 2)))
      ((not (divides? 2 aa)) n)))

(defun my-cdr (z)
  (do ( (n 0 (1+ n))
        (aa z (/ aa 3)))
      ((not (divides? 3 aa)) n)))
;After writing code like this, it becomes immediately obvious that there’s a repetition we can remove by abstracting it as a function:

(defun degree-of-factor (num f)
  "Finds the degree of factor f in number num"
  (do ( (deg 0 (1+ deg))
        (div num (/ div f)))
      ((not (divides? f div)) deg)))

(defun my-car (z)
  (degree-of-factor z 2))

(defun my-cdr (z)