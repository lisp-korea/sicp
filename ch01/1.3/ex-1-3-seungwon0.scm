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
