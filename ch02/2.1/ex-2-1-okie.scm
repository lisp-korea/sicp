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
;; x = 2^a * 3^b

(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (devide? n d)
  (if (= 0 (remainder n d)) #t
      #f))

(define (find-exp x e i)
  (if (devide? x e) (find-exp (/ x e) e (+ i 1))
      i))

(define (my-car x)
  (find-exp x 2 0))

(define (my-cdr x)
  (find-exp x 3 0))
        

;; ex 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (add-1 zero)
;==>
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f x)))

(define two
  (add-1 one)
;==>
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (f x))))

;; text 2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
;; ex 2.7
;(define (make-interval a b) (cons a b))

(define (make-interval lower upper) (cons lower upper))
(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

(define x (make-interval -1.5 1.5))
(define y (make-interval 0.0 10.0))
(add-interval x y)


;; ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(sub-interval x y)

;; ex 2.9
(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(interval-width x)

(= (interval-width (add-interval x y))
   (+ (interval-width x) (interval-width y)))

(= (interval-width (mul-interval x y))
   (* (interval-width x) (interval-width y)))

;; ex 2.10
(define (div-interval x y)
  (if (<= 0 (* (lower-bound y) (lower-bound y)))
      (error "devide by interval containing zero!")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
(div-interval x y)
(div-interval y x)

;; ex 2.11
;; (define (mul-interval x y)
;;   (let ((p1 (* (lower-bound x) (lower-bound y)))
;;         (p2 (* (lower-bound x) (upper-bound y)))
;;         (p3 (* (upper-bound x) (lower-bound y)))
;;         (p4 (* (upper-bound x) (upper-bound y))))
;;     (make-interval (min p1 p2 p3 p4)
;;                    (max p1 p2 p3 p4))))
;;
;;     x  y
;; 1. ++ ++
;; 2. ++ -+
;; 3. ++ --
;; 4. -+ ++
;; 5. -+ -+
;; 6. -+ --
;; 7. -- ++
;; 8. -- -+
;; 9. -- --

(define (mul-interval)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond 
     ;; 1. ++ ++
     ((and (>= lx 0) (>= ux 0) (>= ly 0) (>= uy 0))
      (make-interval (* lx ly) (* ux uy)))
     ;; 2. ++ -+
     ((and (>= lx 0) (>= ux 0) (< ly 0) (>= uy 0))
      (make-interval (* lx ly) (* ux uy)))
     ;; 3. ++ --
     ((and (>= lx 0) (>= ux 0) (< ly 0) (< uy 0))
      (make-interval (* ux ly) (* lx uy)))
     ;; 4. -+ ++
     ((and (>= lx 0) (>= ux 0) (< ly 0) (>= uy 0))
      (make-interval (* lx ly) (* ux uy)))
     ;; 5. -+ -+
     ((and (< lx 0) (>= ux 0) (< ly 0) (>= uy 0))
      (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy))))
     ;; 6. -+ --
     ((and (< lx 0) (>= ux 0) (< ly 0) (< uy 0))
      (make-interval (* ux uy) (* lx uy)))
     ;; 7. -- ++
     ((and (< lx 0) (< ux 0) (>= ly 0) (>= uy 0))
      (make-interval (* ux ly) (* lx uy)))
     ;; 8. -- -+
     ((and (< lx 0) (< ux 0) (< ly 0) (>= uy 0))
      (make-interval (* lx uy) (* ux ly)))
     ;; 9. -- --
     ((and (< lx 0) (< ux 0) (< ly 0) (< uy 0))
      (make-interval (* ux uy) (* lx ly)))
     )))
     
       
                                
        

