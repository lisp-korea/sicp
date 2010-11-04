;; by byulparan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inc a)
  (+ a 1))

(define (dec a)
  (- a 1))

(define (recur+ a b)
  (if (= a 0)
      b
      (inc (recur+ (dec a) b))))

;; (recur+ 5 4)
;; (inc (recur+ 4 4))
;; (inc (inc (recur+ 3 4)))
;; (inc (inc (inc (recur+ 2 4))))
;; (inc (inc (inc (inc (recur+ 1 4)))))
;; (inc (inc (inc (inc (inc (recur+ 0 4))))))
;; (inc (inc (inc (inc (inc 4)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

(define (tail-recur+ a b)
  (if (= a 0)
      b
      (tail-recur+ (dec a) (inc b))))

;; (tail-recur+ 5 4)
;; (tail-recur+ 4 5)
;; (tail-recur+ 3 6)
;; (tail-recur+ 2 7)
;; (tail-recur+ 1 8)
;; (tail-recur+ 0 9)
;; 9


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))


(A 1 10) ; => 1024
(A 2 4) ; => 65536
(A 3 3) ; => 65536


(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

(f 2)
;=> 4
(f 3)
;=> 6
(f 10)
;=> 20
(f 45)
;=> 90

; f(n) =  n * 2

(g 2)
;=> 4
(g 3)
;=> 8
(g 4)
;=> 16

(g 3)
;=> (A 1 3)
;=>(A 0 (A 1 2))
;=>(* 2 (A 0 (A 1 1)))
;=>(* 2 (* 2 2))
;=> (* 2 2 2)
;=> 2^3
(g 4)
;=> (A 1 4)
;=> (A 0 (A 1 3))
;=> (* 2 (A 0 (A 1 2)))
;=> (* 2 2 (A 0 (A 1 1)))
;=> (* 2 2 2 2)
;=> 2 ^ 4

; g(n) = 2 ^ n


(h 1)
;=> 2
(h 2)
;=> 2^h(1) = 4
(h 3)
;=> 2^h(2) = 16
(h 4)
;=> 2^h(3) = 2^16 = 65536
(h 5)
;=> 2^h(4) = 2^65536

(h 1)
; (A 2 1)
; 2

(h 2)
; (A 2 2)
; (A 1 (A 2 1))
; (A 1 (h 1))
;; (A 1 n) == g(n) == 2^n
;; (A 1 (h 1)) == 2^(h 1)

(h 3)
; (A 2 3)
; (A 1 (A 2 2))
; (A 1 (h 2)) == 2^(h 2)

(h 4)
; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (h 3)) ==  2^(h 3)


;; h(1) = 2
;; h(n) = 2 ^ h(n - 1)
;; h(4) == 2 ^ h(3)  == (expt 2 (h 3))
;; (= (expt 2 (h 3)) (h 4))

;; h(5) ==  2 ^ h(4)
;; (= (expt 2 (h 4)) (h 5))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 연습문제 1.11 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; n < 3 == f(n) = n
;; n >= 3 == f(n) = f(n-1) + 2f(n - 2) + 3f(n - 3)


(define (f n)
  (cond ((> 3 n) n)
	(else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))



;;                              f(3)
;;                 (+ (f 2)  (* 2 (f 1))  (* 3 (f 0))) 
;;                  


;;                              f(4)
;;              (+ (f 3) (* 2 (f 2)) (* 3 (f 1)))                    

;;                              f(5)
;;           (+ (f 4)       (* 2 (f 3))       (* 3 (f 2))

;;                              f(6)
;;             (+  (f 5)    (* 2 (f 4))    (* 3 (f 3)))


(define (f-iter n)
  (define (f-inner-iter a b c count)
    (cond ((= count 0) c)
	  (else (f-inner-iter (+ a (* b 2) (* c 3)) a b (- count 1)))))
  (f-inner-iter 2 1 0 n))









