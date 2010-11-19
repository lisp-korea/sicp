;integral 76p

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
    dx))

;; ex 1.29 
  
 (define (cube x) (* x x x)) 
  
 (define (inc n) (+ n 1)) 
  
 (define (sum term a next b) 
   (if (> a b) 
       0 
       (+ (term a) 
          (sum term (next a) next b)))) 
  
 (define (simpson-integral f a b n) 
   (define h (/ (- b a) n)) 
   (define (yk k) (f (+ a (* h k)))) 
   (define (simpson-term k) 
     (* (cond ((or (= k 0) (= k n)) 1) 
              ((odd? k) 4.0) 
              (else 2.0)) 
        (yk k))) 
   (* (/ h 3) (sum simpson-term 0 inc n))) 
  
 ;; Testing 
 (print "ex 1.29")
 (newline)
 (print "integral test")
 (newline)
 (integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(print "simpson-integral test")
(newline)
 (simpson-integral cube 0 1 100) 
 (simpson-integral cube 0 1 1000)
 
 ;ex 1.30
  (define (itersum term a next b) 
  (define (iter a result) 
          (if (> a b) 
              result 
              (iter (next a) (+ result (term a))))) 
  (iter a 0)) 
;to test
  (define (pi-sum a b) 
  (define (pi-term x) 
          (/ 1.0 (* x (+ x 2)))) 
  (define (pi-next x) 
          (+ x 4)) 
  (itersum pi-term a pi-next b)) 
;testing
  (print "ex 1.30")
  (newline)
   (* 8 (pi-sum 1 1000))  

   
   ;ex 1.31
      (define (product term a next b) 
     (if (> a b) 1 
        (* (term a) (product term (next a) next b)))) 
;fomular
 (define (identity x) x) 
  
 (define (next x) (+ x 1)) 
  
 (define (factorial n) 
 (product identity 1 next n)) 

 ; new term
  (define (pi-term n) 
   (if (even? n) 
       (/ (+ n 2) (+ n 1)) 
       (/ (+ n 1) (+ n 2)))) 
;test
  (print "ex 1.31 a")
  (newline)
 (* (product pi-term 1 next 6) 4.0)
 (* (product pi-term 1 next 100) 4.0)
 
;ex 1.32 b
   (define (product-iter term a next b) 
   (define (iter a res) 
     (if (> a b) res 
          (iter (next a) (* (term a) res)))) 
    (iter a 1))
;test
  (print "ex 1.31 b")
  (newline)
  
 (* (product-iter pi-term 1 next 6) 4.0)
 (* (product-iter pi-term 1 next 100) 4.0)
 
 ;ex 1.32
 ;Recursive process 
 (define (accumulate combiner null-value term a next b) 
    (if (> a b) null-value 
        (combiner (term a) (accumulate combiner null-value term (next a) next b)))) 
  ;sum and product as simple calls to acc.
  (define (sum term a next b) (accumulate + 0 term a next b)) 
  (define (product-acc term a next b) (accumulate * 1 term a next b))
  ;test
  (print "ex 1.32 a")
  (newline)
 (* (product-acc pi-term 1 next 6) 4.0)
 (* (product-acc pi-term 1 next 100) 4.0)
 ;1.32 b
   (define (accumulate-iter combiner null-value term a next b) 
    (define (iter a res) 
      (if (> a b) res 
          (iter (next a) (combiner (term a) res)))) 
    (iter a null-value)) 
  ;sum and product as iterative calls to acc.
  (define (sum term a next b) (accumulate-iter + 0 term a next b)) 
  (define (product-acc-iter term a next b) (accumulate-iter * 1 term a next b))
;test
  (print "ex 1.32 b")
  (newline)
 (* (product-acc-iter pi-term 1 next 6) 4.0)
 (* (product-acc-iter pi-term 1 next 100) 4.0)
 
 ;ex 1.33
   (define (smallest-div n) 
    (define (divides? a b) 
      (= 0 (remainder b a))) 
    (define (find-div n test) 
       (cond ((> (sq test) n) n) ((divides? test n) test) 
             (else (find-div n (+ test 1))))) 
    (find-div n 2)) 
  ;prime? define modification!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   (define (prime? n) 
     (if (< 1 n) 
        (= n (smallest-div n))
        #f
        )
   ) 
;filtered acc.
     (define (filtered-accumulate combiner null-value term a next b filter) 
  (if (> a b) null-value 
      (if (filter a) 
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter)) 
          (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filter)))))
     ;ex 1.33 a
     (define (sq x) (* x x))
       (define (sum-of-prime-squares a b) (filtered-accumulate + 0 sq a inc b prime?)) 
;test
       (print "ex 1.33 a")
       (newline)
       (sum-of-prime-squares 1 5)
       
       ;ex 1.33 b
(define (filtered-accumulate-iter combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
       result
       (if (filter a b)
          (iter (next a) (combiner (term a) result))
          (iter (next a) result))))
  (iter a null-value))
       
(define (gcd a b)
  (if (= b 0)
     a
     (gcd b (remainder a b))))
(define (gcd1? i n) (= (gcd i n) 1))

;test
(print "ex 1.33 b")
(newline)
(filtered-accumulate + 0 sq 2 inc 10 prime?)
(filtered-accumulate-iter * 1 identity 1 inc 10 gcd1?)

;ex 1.34
(define (f g)
  (g 2))
(f sq)
(f (lambda (z) (* z (+ z 1))))
;(f f) is error ; argument must be procedure (e. g. + - * or sq, etc)

;ex 1.35
; funtion fixed point of f(x) is x at f(x) =x .
; Thus, x is fixed point at '1 + 1/x = x' 
; 1 + 1/x = x ====> x^2 = x + 1
; At this, x^2 = x + 1 is fomular for golden ratio(We can check 1.2.2(50p)
;Therefore, golden ratio is fixed point of 'x |-> 1 + 1/x'

; fixed-point procedure calculation
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2) ) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
         next
         (try next))))
  (try first-guess)
  )
;result
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

