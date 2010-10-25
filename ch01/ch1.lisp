;; ex 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
            (* 3 (- 6 2) (- 2 7)))

;; ex 1.3
(defun square (x)
  (* x x))

(defun sum-of-two-sqaures (x y)
  (+ (square x) (square y)))

;; solution 1
;; (defun sum-of-larger-two (x y z)
;;            (if (> x y)
;;                (if (> y z)
;;                    (sum-of-two-sqaures x y)
;;                    (sum-of-two-sqaures x z))
;;                (if (> x z)
;;                    (sum-of-two-sqaures y x)
;;                    (sum-of-two-sqaures y z))))

;; solution 2
;; (defun sum-of-larger-two (x y z)
;;   (if (>= x y)
;;       (sum-of-two-sqaures x (if (>= y z) y z))
;;       (sum-of-two-sqaures y (if (>= x z) x z))))
      

;; solution 3
(defun smallest-of-three (x y z)
  (if (> x y)
      (if (> y z) z y)
      (if (> x z) z x)))

(defun sum-of-larger-two (x y z)
  (+ (square x) (square y) (square z) (- (square (smallest-of-three x y z)))))

;; solution 4
(defun sum-of-larger-two (a b c)
  (+ (if (or (> a b) (> a c))
         (square a)
         0)
     (if (or (> b c) (> b a))
         (square b)
         0)
     (if (or (> c a) (> c b))
         (square c)
         0)))

;; ex 1.4
(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) #'+ #'-) a b))

(defun a-plus-abs-b (a b)
  (funcall (if (> b a) (function +) (function -)) a b))

;; ex 1.5
(defun p ()
    (p))

;; Lisp nesting exceeds `max-lisp-eval-depth'

(defun test (x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;; ex 1.6
(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun sqrt-iter (g x)
  (if (good-enough? g x)
      g
      (sqrt-iter (improve g x)
                 x)))

(defun mysqrt (x)
           (sqrt-iter 1.0 x))

;; ex 1.7
;; solution 1
(defun guess-enough? (g prev-guess)
  (< (abs (- g prev-guess)) (* g 0.001)))

(defun sqrt-iter (g prev-guess x)
  (if (guess-enough? g prev-guess)
      g
      (sqrt-iter (improve g x) g
                 x)))

(defun mysqrt (x)
           (sqrt-iter 1.0 0.0 x))

;; soluation 2
(defun close-enough? (a b)
  (< (abs (- 1 (/ a b))) 0.001))

(defun sqrt-iter (g x)
  (let ((improved-guess (improve g x)))
    (if (close-enough? g improved-guess)
        improved-guess
        (sqrt-iter improved-guess x))))
  
(defun mysqrt (x)
           (sqrt-iter 1.0 x))

;; ex 1.8
(defun cbrt-improve (guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(defun cbrt-iter (g x)
  (let ((improved-guess (cbrt-improve g x)))
    (if (close-enough? g improved-guess)
        improved-guess
        (cbrt-iter improved-guess x))))

(defun cbrt (x)
  (cbrt-iter 1.0 x))


;; ex 1.9
(defun plus-recur (a b)
  (declare (NOTINLINE PLUS-RECUR))
  (if (= a 0)
      b 
      (1+ (plus-recur (1- a) b))))

; (plus-recur 4 5)
; (1+ (plus-recur 3 5))
; (1+ (1+ (plus-recur 2 5)))
; (1+ (1+ (1+ (plus-recur 1 5))))
; (1+ (1+ (1+ (1+ (plus-recur 0 5)))))
; (1+ (1+ (1+ (1+ 5))))

(defun plus-iter (a b)
  (declare (NOTINLINE PLUS-ITER))
  (if (= a 0)
      b
      (plus-iter (1- a) (1+ b))))

; (plus-iter 4 5)
; (plus-iter 3 6)
; (plus-iter 2 7)
; (plus-iter 1 8)
; (plus-iter 0 9)
; 9

;; ex 1.10
(defun A (x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (t (A (- x 1)
              (A x (- y 1))))))

; (A 1 10)
; (A 0 (A 1 9))
; (* 2 (A 1 9))
; (* 2 (A 0 (A 1 8)))
; (* 2 (* 2 (A 1 8)))
; (* 2 (* 2 (A 0 (A 1 7))))
; ...
; (* 2 (* 2 (* ... x n)))
; 2 ^ 10

; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (* 2 (A 1 3)))
; (A 1 (* 2 * 8))
; 2 ^ 16

(defun f (n) (A 0 n))
; 2 * n

(defun g (n) (A 1 n))
; 2 ^ n

(defun h (n) (A 2 n))
; 2 ^ (2 ^ n)

(defun k (n) (* 5 n n))

;; text 1.2.2
(defun count-change (amount)
  (cc amount 5))

(defun cc (amount kinds-of-coins)
  (declare (NOTINLINE CC))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (t (+ (cc amount
                  (- kinds-of-coins 1))
              (cc (- amount
                     (first-denomination kinds-of-coins))
                  kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; ex 1.11
(defun f-recur (n)
  (declare (NOTINLINE f-recur))
  (cond ((< n 3) n)
        ((>= n 3) (+ (f (- n 1))
                     (* 2 (f (- n 2)))
                     (* 3 (f (- n 3)))))))

(defun f-iter-internal (n)
  (cond ((< n 3) n)
        ((>= n 3)

(defun f-iter (n)
  (declar (NOTINLINE f-iter)))

(trace f-iter)