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

;; 1.2.5

(defun gcd-recur (a b) 
;  (print a b)
  (format t "GCD : ~$ ~$~%" a b)
  (if (= b 0)
      a
      (gcd-recur b (mod a b))))

;; ex 1.20

; applicative-order
(gcd-recur 206 40)
(gcd-recur 40 (mod 206 40))
(gcd-recur 40 6)
(gcd-recur 6 (mod 40 6))
(gcd-recur 6 4)
(gcd-recur 4 (mod 6 4)) 
(gcd-recur 4 2)
(gcd-recur 2 (mod 4 2)) 
(gcd-recur 2 0)
2

;; text 1.2.6
(defun even? (n)
  (if (= 0 (mod n 2))
      t
      nil))

(defun square (x)
  (* x x))

(defun expmod (base exponent m)
  (declare (NOTINLINE EXPMOD))
  (cond ((= exponent 0) 1)
        ((even? exponent)
          (mod  (square (expmod base (/ exponent 2) m))
                m))
        (t
          (mod  (* base (expmod base (- exponent 1) m))
                m))))

;; ex 1.21
(defun divides? (a b)
  (= (mod b a) 0))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (t (find-divisor n (+ test-divisor 1)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun prime? (n)
  (= n (smallest-divisor n)))

(dolist (num '(199 1999 19999))
  (format t
    "The smallest divisor of ~d is ~d~%"
    num (smallest-divisor num)))

;; ex 1.22
(defun search-for-primes (start end)
  (let ((start (if (evenp start) (1+ start) start)))
    (do ((i start (+ i 2)))
        ((> i end))
      (when (prime? i)
        (format t "~d is prime~%" i)))))

(time (dotimes (i 1000 t) (search-for-primes 1000 1019)))

;; ex 1.23
(defun next-divisor (n)
  (if (= n 2)
    3
    (+ n 2)))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (t (find-divisor n (next-divisor test-divisor)))))

;; ex 1.24
;; O(log(n))

;; ex 1.25
;; fast-expt will make huge numbers

;; ex 1.26
(trace expmod)
(expmod 15 10 10)

(defun louis-expmod (base exponent m)
  (declare (NOTINLINE LOUIS-EXPMOD))
  (cond ((= exponent 0) 1)
        ((evenp exponent)
          (rem  (*  (louis-expmod base (/ exponent 2) m)
                    (louis-expmod base (/ exponent 2) m))
                m))
        (t
          (rem  (* base (louis-expmod base (- exponent 1) m))
                m))))

(trace louis-expmod)
(louis-expmod 15 10 10)


;; ex 1.27
;; basic
(defun full-fermat-test (n)
  (defun aux-test (a)
    (cond ((= a 1) t)
          ((/= (expmod a n n) a) nil)
          (t (aux-test (1- a)))))
  (aux-test (1- n)))

;; tail recursion by compiling aux-test in cl
(defun full-fermat-test (n)
  (defun aux-test (a)
    (cond ((= a 1) t)
          ((/= (expmod a n n) a) nil)
          (t (aux-test (1- a)))))
  (compile 'aux-test)
  (aux-test (1- n)))

;; ex 1.28
(defun expmod (base exponent m)
  (cond ((= exponent 0) 1)
        ((evenp exponent)
          (let* ( (candidate (expmod base (/ exponent 2) m))
                  (root (rem (square candidate) m)))
                (if (and (/= candidate 1) (/= candidate (1- m)) (= root 1))
                  0
                  root)))
        (t
          (rem  (* base (expmod base (- exponent 1) m))
                m))))

(defun miller-rabin-test (n)
  (let ((testnum (1+ (random (1- n)))))
    (= (expmod testnum (1- n) n) 1)))