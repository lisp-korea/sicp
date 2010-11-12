;;;
;;; File: ex-1-2-okie.lisp
;;;

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

(defun fib(n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))
;; a <- a + b
;; b <- a

;; (fib 1) + (fib 0), count : 1
;; (fib 2) + (fib 1), count : 2
;; (fib 3) + (fib 2), count : 3
;; (fib 4) + (fib 3), count : 4
;; (fib 5) + (fib 4),

(defun fib-iter(a b count)
 (declare (NOTINLINE fib-iter))
  (if (= count 1)
      a
      (fib-iter (+ a b) a (- count 1))))

(defun fib(n)
  (fib-iter 1 0 n))


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
        ((>= n 3) (+ (f-recur (- n 1))
                     (* 2 (f-recur (- n 2)))
                     (* 3 (f-recur (- n 3)))))))


;n < 3 : f(n) = n
;n >=3 : f(n) = f(n-1) + 2 * f(n-2) + 3 * f(n-3)

;; f(0) = 0
;; f(1) = 1
;; f(2) = 2
;; f(3) = f(2) + 2f(1) + 3f(0) count : 1
;; f(4) = f(3) + 2f(2) + 3f(1) count : 2
;; f(5) = f(4) + 2f(3) + 3f(2) count : 3
;; f(6) = f(5) + 2f(4) + 3f(3) count : 4
;; f(7) = f(6) + 2f(5) + 3f(4) count : 5
;; f(8) = f(7) + 2f(6) + 3f(5) count : 6

;; a <- a + b + c
;; b <- a
;; c <- b

(defun f-iter-internal (a b c count)
  (declare (NOTINLINE f-iter-internal))
  (if (= count 2)
        a ;; count = 2, count = 0(c)
        (f-iter-internal (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(defun f-iter (n)
  (f-iter-internal 2 1 0 n))

(trace f-iter)

; ex 1.12

; ex 1.13
