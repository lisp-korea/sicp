

;=========== 1.2 =============================
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 2 3))))) (* 3 (- 6 2) (- 2 7)))

;============= 1.3 =============================

(define (square a) (* a a))

(define (ex3 x y z)
  (define (smallest a b c)
    (cond ((and (<= a b) (<= a c)) a)
          ((and (<= b a) (<= b c)) b)
          ((and (<= c a) (<= c b)) c)
          (else (error "I don't know" a b c))))
  (cond ((= (smallest x y z) x) (+ (square y) (square z)))
        ((= (smallest x y z) y) (+ (square x) (square z)))
        ((= (smallest x y z) z) (+ (square x) (square y)))))


;========== 1.4 ===============
(define (a-plus-abs-b a b)
  ((if (> b 0) 
       +
       -)
   a b))
(a-plus-abs-b 3 -1)
(a-plus-abs-b 4 4)

;============= 1.5 =================
(define (p) (p))

(define (test x y)
  (if (= x 0) 0
      y))

;; guile do process parameter
;; so it will do infinite-loop to do p->p->p->....
(test 0 (p))


;=========== 1.6 ===================
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (display guess)
  (newline)
  (< (abs (- (square guess) x)) 0.001))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (square x)
  (* x x))

(sqrt-iter 1.0 2.0)


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause) 
        (else else-clause)))

(new-if (= 2 3) 1 0)
(new-if (= 3 3) 1 0)

(new-if (good-enough? 0.001 0.0001) 1 0)


;; using new-if
;; new-if is procedure, so that new-sqrt-iter (~~) parameter is
;; processed first. Finally new-sqrt-iter will do infinite-loop.
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x) 
          guess
          (new-sqrt-iter (improve guess x) x)))
(new-sqrt-iter 1 2) ; not working

;; using only the body of new-if
(define (nn-sqrt-iter guess x)
  (cond ((good-enough? guess x) guess)
        (else (nn-sqrt-iter (improve guess x) 
                            x))))
(nn-sqrt-iter 1.0 2.0) ; working

;================= 1.7 ===================================

(define limit 0.0000001)
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))
(define (square x)
  (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) limit))

(sqrt-iter 1 0.001)


(define (new-good-enough? new-guess old-guess)
  (< (abs (- new-guess old-guess)) limit))
(define (new-sqrt-iter guess x)
  (let ((new-guess (improve guess x)))
    (if (new-good-enough? new-guess guess)
        guess
        (sqrt-iter new-guess x))))
(new-sqrt-iter 1 0.001)

;; How about precision??
(new-sqrt-iter 1 0.00005)
(sqrt-iter  1 0.00005)



;====== ex1.8 ===============
(define limit 0.1)

(define (cube x) (* (* x x) x))

(define (good-enough? guess x)
  (display guess) (newline)
  (< (abs (- (cube guess) x)) limit))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x) x)))

(cube-iter 1.0 8.0)


;====== ex 1.9 ===============

;; If inc calls +, it will be recursive procedure of +,
;; and it will be infinite loop.
;; Thefore I make non-recursive version of inc.
(define (recur-inc x) (+ x 1))
(define (no-recur-inc x)
  (cond ((= x 0) 1)
        ((= x 1) 2)
        ((= x 2) 3)
        ((= x 3) 4)
        ((= x 4) 5)
        ((= x 5) 6)
        ((= x 6) 7)
        ((= x 7) 8)
        ((= x 8) 9)
        ((= x 9) 10)))
(define (dec x) (- x 1))

(define (+ a b)
  (if (= a 0)
      b
      (recur-inc (+ (dec a) b))))
;; (+ 4 5) ->
;; (inc (+ (dec 4) 5)) ->
;; (inc (+ 3 5)) ->
;; (inc (inc (+ 2 5))) ->
;; (inc (inc (inc (+ 1 5)))) ->
;; (inc (inc (inc (inc (+ 0 5))))) ->
;; (inc (inc (inc (inc 5)))) ->
;; (inc (inc (inc (+ 5 1)))) ->
;; (inc (inc (inc (inc (+ 4 1))))) ->
;; (...................(inc (+ 3 1))) ->
;; (........................(inc (+ 2 1))) ->
;; (........................(inc (inc (+ 1 1)))) ->
;; (..................................(inc (+ 0 1))) ->
;; (.......................................(... !!!!
;; (inc 1) ==> (+ 1 1) ==> (inc (+ 0 1)) ==> (inc 1) ==> infinite loop!!!

(define (+ a b)
  (if (= a 0)
      b
      (no-recur-inc (+ (dec a) b)))))

;; (+ 2 3)
;; (inc (+ 1 3))
;; (inc (inc (+ 0 3)))
;; (inc (inc 3))
;; (inc 4)
;; 5
;; Using no-recur-inc, it does not infinite loop!!

;; This is the deferered operation & linearly increasing operation.
;; This is linear recursive process!


(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; using recur-inc
;; (+ 4 5) ->
;; (+ (dec 4) (inc 5)) ->
;; (+ (- 4 1) (+ 5 1)) ->
;; (+ 3 (+ 4 (inc 1))) ->
;; (+ 3 (+ 4 (+ 1 1))) ->
;; (+ 3 (+ 4 (+ 0 (inc 1)))) ->
;; (..............(+ 1 1)) ->
;; (..............(+ (inc 1))) -> !!!
;; (+ 1 1) -> (+ 0 (inc 1)) -> (+ 0 (+ 1 1)) -> infinitly increasing
;;
;; using no-recur-inc
;; (+ 4 3)
;; (+ 3 (inc 3))
;; (+ 3 4)
;; (+ 2 (inc 4))
;; (+ 1 (inc 5)
;; (+ 0 (inc 6))
;; 7
;; It calls inc procedure at 4 times.
;; The first argument is the state variable and
;; calling inc is at the number of the first argument.
;; This is linear iterative process.



;====== ex 1.10 ============
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;; guile> 1024
;; guile> 65536
;; guile> 65536
(A 1 10)
(A 2 4)
(A 3 3)



;; (A 1 10) = (A 0 (A 1 9)) = (A 0 (A 0 (A 1 8)))=(A 0 (A 0 (A...(A 1 1))))
;; = (A 0 (...(A 0 2)))
;; = (A 0 (...4))
;; = (A 0 (A 0 (A ... (A 0 2)))) ; 2 * 2^9 = 2^10
;; therefore, (A 1 n) is 2^n

;; (A 2 4) = (A 1 (A 2 3)) = (A 1 (A 1 (A 2 2))) = (A 1 (A 1 (A 1 (A 2 1)))) = (A 1 (A 1 (A 1 2)))
;; ==> (A 1 2) = 2^2
;; ==> (A 1 (A 1 2)) = 2 ^ (2^2)
;; ==> (A 1 (A 1 (A 1 2))) = 2 ^ (2 ^ (2^2)) = 2^16 = 65536 => 2^2^2^2
;; therefore, (A 2 n) is 2^2^...^2 (n-times)
;; (A 2 3) -> 2^2^2=2^4=16

(define (f n) (A 0 n)) ;; 2n            
(define (g n) (A 1 n)) ;; 2^n
(define (h n) (A 2 n)) ;; a(n) = 2^a(n-1), (n=1, a(1)=2)
(define (k n) (* 5 n n)) ;; 5n^2



;=========== ex 1.11 ============

; recursive

(define (ex1_11_recur n)
  (if (< n 3) n
      (+ (ex1_11 (- n 1)) 
         (* 2 (ex1_11 (- n 2)))
         (* 3 (ex1_11 (- n 3))))))

(ex1_11_recur 1)
(ex1_11_recur 2)
(ex1_11_recur 3)
(ex1_11_recur 4)
(ex1_11_recur 5)
(ex1_11_recur 6)

; iterative

; count = 3
; n >= 3
; f1=2, f2=1, f3=0
; 
(define (ex1_11_iter n)
  (ex1_11_iter_body 3 n 2 1 0)) ;; initial value of count is 3

(define (ex1_11_iter_body count n f_n-1 f_n-2 f_n-3)
  (cond ((< n 3) n) ; n<3, result = n
        ((= count n) (+ f_n-1 (* 2 f_n-2) (* 3 f_n-3))) ; f(n) = f(n-1)+2*f(n-2)+3*f(n-3)
        (else (ex1_11_iter_body (+ count 1) ; increase count
                           n           ; n 
                           (+ f_n-1 (* 2 f_n-2) (* 3 f_n-3)) ; f(n) -> f(n-1)
                           f_n-1                       ; f(n-1) -> f(n-2)
                           f_n-2))))                   ; f(n-2) -> f(n-3)

(ex1_11_iter 1)
(ex1_11_iter 2)
(ex1_11_iter 3)
(ex1_11_iter 4)
(ex1_11_iter 5)
(ex1_11_iter 6)


;============= 1.12 ====================
;;     1         ; level 1
;;    1 1        ; level 2
;;   1 2 1       ; ...
;;  1 3 3 1
;; 1 4 6 4 1
;; ...
;; 1 2 3 4 5     ; order

;;RESULT: value of (level=l, order=o) = value of ((level=l-1, order=o-1) + (level=l-1, order=o))


(define (get-num level order)
  (cond ((< level order) -1)
        ((= order 1) 1)
        ((= order level) 1)
        (else (+ (get-num (- level 1) (- order 1))
                 (get-num (- level 1) order)))))

(get-num 1 1)
(get-num 2 1)
(get-num 2 2)
(get-num 3 1)
(get-num 3 2)
(get-num 3 3)
(get-num 4 1)
(get-num 4 2)
(get-num 4 3)
(get-num 4 4)
(get-num 4 5) ;; error!


(define (print-pascal-triangle-line level)
  (define (print-pascal-triangle-line-body order level)
    (if (<= order level) (begin (display (get-num level order))
                                (display " ")
                                (print-pascal-triangle-line-body (+ order 1) level))
        ))
  (print-pascal-triangle-line-body 1 level))

(define (print-pascal-triangle level)
  (if (> level 0) (print-pascal-triangle (- level 1))) ;; no else-statement
  ;; if level is 0, it starts to print lines from 0-level to specified level.
  (print-pascal-triangle-line level) 
  (newline))
      
(print-pascal-triangle 0)
(print-pascal-triangle 1)
(print-pascal-triangle 2)
(print-pascal-triangle 3)
(print-pascal-triangle 4)
(print-pascal-triangle 5)
(print-pascal-triangle 6)




