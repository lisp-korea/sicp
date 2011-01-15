;; ex-1.29
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)                ;.24998750000000042
(integral cube 0 1 0.001)               ;.249999875000001

(define (inc n) (+ n 1))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (simpsons-rule-term k)
    (* (cond ((= k 0) 1)
             ((= k n) 1)
             ((even? k) 2)
             (else 4))
     (f (+ a (* k h)))))
  (* (/ h 3) (sum simpsons-rule-term 0 inc n)))

(simpsons-rule cube 0 1  100.0)                ;.24999999999999992
(simpsons-rule cube 0 1 1000.0)                ;.2500000000000003


;; ex-1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


;; ex-1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (term1 x)
  (if (even? x)
      x
      (+ x 1)))

(define (term2 x)
  (if (even? x)
      (+ x 1)
      x))

(* 4.0
   (/ (product term1 2 inc 99)
      (product term2 2 inc 99)))	;3.157339689217565

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


;; ex-1.32
(accumulate combiner null-value term a next b)

(define (accumulate combiner null-value term a next b)
    (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))


;; ex-1.33
(define (filtered-accmulate combiner null-value term a next b predicate?)
    (if (> a b)
      null-value
      (combiner (if (predicate? a)
		    (term a)
		    null-value)
		(filtered-accumulate combiner null-value term (next a) next b predicate?))))

(filtered-accmulate + 0 square a inc b prime?)

(filtered-accmulate * 1 ident 0 inc n (= (gcd i n) 1))


;; ex-1.34
(define (f g) (g 2))

(f square)				;4

(f (lambda (z) (* z (+ z 1))))		;6

(f f)					;The object 2 is not applicable.
(f 2)
(2 2)


;; ex-1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ;1.6180327868852458


;; ex-1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0) ;4.555532270803653

;; 9.965784284662087
;; 3.004472209841214
;; 6.279195757507157
;; 3.759850702401539
;; 5.215843784925895
;; 4.182207192401397
;; 4.8277650983445906
;; 4.387593384662677
;; 4.671250085763899
;; 4.481403616895052
;; 4.6053657460929
;; 4.5230849678718865
;; 4.577114682047341
;; 4.541382480151454
;; 4.564903245230833
;; 4.549372679303342
;; 4.559606491913287
;; 4.552853875788271
;; 4.557305529748263
;; 4.554369064436181
;; 4.556305311532999
;; 4.555028263573554
;; 4.555870396702851
;; 4.555315001192079
;; 4.5556812635433275
;; 4.555439715736846
;; 4.555599009998291
;; 4.555493957531389
;; 4.555563237292884
;; 4.555517548417651
;; 4.555547679306398
;; 4.555527808516254
;; 4.555540912917957
;; 4.555532270803653

(define (average a b) (/ (+ a b) 2))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2.0)                       ;4.555537551999825

;; 5.9828921423310435
;; 4.922168721308343
;; 4.628224318195455
;; 4.568346513136242
;; 4.5577305909237005
;; 4.555909809045131
;; 4.555599411610624
;; 4.5555465521473675
;; 4.555537551999825


;; 1.37
;; a
(define phi 1.6180327868852458)

(/ 1 phi)                ;.6180344478216819

(define (cont-frac n d k)
  (define (cont-frac-iter x v)
    (define frac (/ (n x) (+ (d x) v)))
    (cond ((= x 1) frac)
          (else (cont-frac-iter (- x 1) frac))))
  (cont-frac-iter k 0))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1) ;1.
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 2) ;.5
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 3) ;.6666666666666666
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10) ;.6179775280898876
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11) ;.6180555555555556

;; b
(define (cont-frac n d k)
  (define (cont-frac-recurse x)
    (cond ((= x k) (/ (n x) (d x)))
          (else (/ (n x) (+ (d x)
                            (cont-frac-recurse (+ x 1)))))))
  (cont-frac-recurse 1))


;; ex-1.38
(define (n x) 1)
(define (d x)
  (cond ((<= x 2) x)
        ((= (remainder (- x 2) 3) 0)
         (+ 2 (* (/ (- x 2) 3) 2)))
        (else 1)))
(+ 2.0 (cont-frac n d 5))               ;2.71875
(+ 2.0 (cont-frac n d 10))              ;2.7182817182817183
(+ 2.0 (cont-frac n d 20))              ;2.718281828459045


;; ex-1.39
(define (tan-cf x k)
  (define (n y) (if (= y 1) x (- (* x x))))
  (define (d y) (- (* 2 y) 1))
  (cont-frac n d k))


;; ex-1.40
