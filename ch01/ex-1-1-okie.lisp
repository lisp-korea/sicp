;; sicp solution ch 1.1 by okie

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
  (i(f (= x 0)
      0
      y)))

(test 0 (p))

;; TODO : find lazy evaluation package
;; scheme : (require lazy)

;; ex 1.6
(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (print guess)
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
  (print g)
  (< (abs (- g prev-guess)) (* g 0.001)))

(defun sqrt-iter (g prev-guess x)
  (if (guess-enough? g prev-guess)
      g
      (sqrt-iter (improve g x) g
                 x)))

(defun mysqrt (x)
           (sqrt-iter 1.0 0.0 x))

;; solution 2
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