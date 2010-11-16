;; 연습문제 1.29

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))



(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (y k) (cond ((or (= k 0) (= k n)) 1)
		   ((odd? k) 4)
		   (else 2))))
  (* (/ h 3.0) (sum term 0 inc n)))


(simpson cube 0 1 100)
;=> 0.25
(simpson cube 0 1 1000)
;=> 0.25



;; 연습문제 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))


(simpson cube 0 1 100)
;=> 0.25
(simpson cube 0 1 1000)
;=> 0.25



;; 연습문제 1.31
;; a.
(define (product f a next b)  ;; recursive
  (if (> a b)
      1
      (* (f a) (product f (next a) next b))))

(define (my-pi n)  
  (define (term k)
    (/ (+ k (if (odd? k) 1 2))
       (+ k (if (odd? k) 2 1))))
  (* 4.0 (product term 1 inc n)))

(my-pi 100)
;=> 3.1570301764551676
(my-pi 1000)
;=> 3.1431607055322663

;; b. iterate
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (my-pi n)  
  (define (term k)
    (/ (+ k (if (odd? k) 1 2))
       (+ k (if (odd? k) 2 1))))
  (* 4.0 (product-iter term 1 inc n)))


(my-pi 100)
;=> 3.1570301764551676
(my-pi 1000)
;=> 3.1431607055322663

;연습문제 1.32
;; a. recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))
;=> (sum cube 0 inc 10)

(define (product term a next b)
  (accumulate * 1 term a next b))
;=> (product cube 1 inc 10)

;; b. iter
(define (accumulate-iter combiner null-value term a next b)
  (define (iter k result)
    (if (> k b)
	result
	(iter (next k) (combiner result (term k)))))
  (iter a null-value))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(= (product-iter cube 1 inc 10) (product cube 1 inc 10))
;=> #t

