

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

