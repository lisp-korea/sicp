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
;; 1.11
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.14 ???

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.15 ???

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.16

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.17 ???

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.18 ???

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.19

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.20

;; if is special-form
;;
;; ;;  normal-order evailation

; remainder count: 0
(gcd 206 40)

; rc: 0
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40))) 

; rc: 0
(gcd 40 (remainder 206 40)) 

; rc: 0
(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40)
         (remainder 40 (remainder 206 40))))

; rc: 1
(if (= 6 0)
    40
    (gcd (remainder 206 40)
         (remainder 40 (remainder 206 40))))

; rc: 1
(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40)))

; rc: 1
(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))))

; rc: 2
(if (= (remainder 40 6) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))))

; rc: 3
(if (= 4 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))))

; rc: 3
(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40
                           (remainder 206 40))))

; rc: 3
(if (= (remainder (remainder 206 40)
                  (remainder 40
                             (remainder 206 40)))
       0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))))

; rc: 4
(if (= (remainder 6
                  (remainder 40
                             (remainder 206 40)))
       0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))))

; rc: 5
(if (= (remainder 6
                  (remainder 40
                             6))
       0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))))

; rc: 6
(if (= (remainder 6 4) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))))

; rc: 7
(if (= 2 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))))

; rc: 7
(gcd (remainder (remainder 206 40)
                (remainder 40
                           (remainder 206 40)))
     (remainder (remainder 40
                           (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40
                                      (remainder 206 40)))))

; rc: 7
(if (= (remainder (remainder 40
                             (remainder 206 40))
                  (remainder (remainder 206 40)
                             (remainder 40
                                        (remainder 206 40))))
       0)
    (remainder (remainder 206 40)
               (remainder 40
                          (remainder 206 40)))
    (gcd (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40)))
                    (remainder (remainder 40
                                          (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))

; rc: 14 (there were 7 calls to remainder in the = form above)
(if (= 0 0)
    (remainder (remainder 206 40)
               (remainder 40
                          (remainder 206 40)))
    (gcd (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40)))
                    (remainder (remainder 40
                                          (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))

; rc: 14
(remainder (remainder 206 40)
           (remainder 40
                      (remainder 206 40)))

; rc: 15
(remainder 6
           (remainder 40
                      (remainder 206 40)))

; rc: 16
(remainder 6 (remainder 40 6))

; rc: 17
(remainder 6 4)

; rc: 18
2

;; ;;  applicative-order evailation


; rc: 0
(gcd 206 40)

; rc: 0
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40))) 

; rc: 0
(gcd 40 (remainder 206 40))

; rc: 1
(gcd 40 6)

; rc: 1
(if (= 6 0)
    40
    (gcd 6 (remainder 40 6))) 

; rc: 1
(gcd 6 (remainder 40 6))

; rc: 2
(gcd 6 4)

; rc: 2
(if (= 4 0)
    6
    (gcd 4 (remainder 6 4))) 

; rc: 2
(gcd 4 (remainder 6 4))

; rc: 3
(gcd 4 2)

; rc: 3
(if (= 2 0)
    4
    (gcd 2 (remainder 4 2))) 

; rc: 3
(gcd 2 (remainder 4 2))

; rc: 4
(gcd 2 0)

; rc: 4
(if (= 0 0)
    2
    (gcd 0 (remainder 2 0))) 

; rc: 4
2


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.21
