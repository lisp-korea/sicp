; ex 1.21
; > (smallest-divisor 199) 
; 199 
; > (smallest-divisor 1999) 
; 1999 
; > (smallest-divisor 19999) 
; 7 

;; ex 1.22 
  
 (define (square x) (* x x)) 
  
 (define (smallest-divisor n) 
   (find-divisor n 2)) 
  
 (define (find-divisor n test-divisor) 
   (cond ((> (square test-divisor) n) n) 
         ((divides? test-divisor n) test-divisor) 
         (else (find-divisor n (+ test-divisor 1))))) 
  
 (define (divides? a b) 
   (= (remainder b a) 0)) 
  
 (define (prime? n) 
   (= n (smallest-divisor n))) 
  
 (define (timed-prime-test n) 
   (start-prime-test n (runtime))) 
  
 (define (start-prime-test n start-time) 
   (if (prime? n) 
       (report-prime n (- (runtime) start-time)))) 
  
 (define (report-prime n elapsed-time) 
   (newline) 
   (display n) 
   (display " *** ") 
   (display elapsed-time)) 
  
 (define (search-for-primes first last) 
   (define (search-iter cur last) 
     (if (<= cur last) (timed-prime-test cur)) 
     (if (<= cur last) (search-iter (+ cur 2) last))) 
   (search-iter (if (even? first) (+ first 1) first) 
                (if (even? last) (- last 1) last))) 
  
 (search-for-primes 1000 1019)       ; 1e3 
 (search-for-primes 10000 10037)     ; 1e4 
 (search-for-primes 100000 100043)   ; 1e5 
 (search-for-primes 1000000 1000037) ; 1e6 
  
 ; As of 2008, computers have become too fast to appreciate the time 
 ; required to test the primality of such small numbers. 
 ; To get meaningful results, we should perform the test with numbers 
 ; greater by, say, a factor 1e6. 
 (newline) 
 (search-for-primes 1000000000 1000000021)       ; 1e9 
 (search-for-primes 10000000000 10000000061)     ; 1e10 
 (search-for-primes 100000000000 100000000057)   ; 1e11 
 (search-for-primes 1000000000000 1000000000063) ; 1e12 

 ;; ex 1.23 
  
 (define (square x) (* x x)) 
  
 (define (smallest-divisor n) 
   (find-divisor n 2)) 
  
 (define (find-divisor n test-divisor) 
   (define (next n) 
     (if (= n 2) 3 (+ n 2))) 
   (cond ((> (square test-divisor) n) n) 
         ((divides? test-divisor n) test-divisor) 
         (else (find-divisor n (next test-divisor))))) 
  
 (define (divides? a b) 
   (= (remainder b a) 0)) 
  
 (define (prime? n) 
   (= n (smallest-divisor n))) 
  
 (define (timed-prime-test n) 
   (start-prime-test n (runtime))) 
  
 (define (start-prime-test n start-time) 
   (if (prime? n) 
       (report-prime n (- (runtime) start-time)))) 
  
 (define (report-prime n elapsed-time) 
   (newline) 
   (display n) 
   (display " *** ") 
   (display elapsed-time)) 
  
 (timed-prime-test 1009) 
 (timed-prime-test 1013) 
 (timed-prime-test 1019) 
 (timed-prime-test 10007) 
 (timed-prime-test 10009) 
 (timed-prime-test 10037) 
 (timed-prime-test 100003) 
 (timed-prime-test 100019) 
 (timed-prime-test 100043) 
 (timed-prime-test 1000003) 
 (timed-prime-test 1000033) 
 (timed-prime-test 1000037) 
  
 ; See comments in exercise 1.22 
 (newline) 
 (timed-prime-test 1000000007) 
 (timed-prime-test 1000000009) 
 (timed-prime-test 1000000021) 
 (timed-prime-test 10000000019) 
 (timed-prime-test 10000000033) 
 (timed-prime-test 10000000061) 
 (timed-prime-test 100000000003) 
 (timed-prime-test 100000000019) 
 (timed-prime-test 100000000057) 
 (timed-prime-test 1000000000039) 
 (timed-prime-test 1000000000061) 
 (timed-prime-test 1000000000063) 


;; ex 1.24 
  
 (define (square x) (* x x)) 
  
 (define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (remainder (square (expmod base (/ exp 2) m)) 
                     m)) 
         (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m))))         
  
 (define (fermat-test n) 
   (define (try-it a) 
     (= (expmod a n n) a)) 
   (try-it (+ 1 (random (- n 1))))) 
  
 (define (fast-prime? n times) 
   (cond ((= times 0) true) 
         ((fermat-test n) (fast-prime? n (- times 1))) 
         (else false))) 
  
 (define (prime? n) 
   ; Perform the test how many times? 
   ; Use 100 as an arbitrary value. 
   (fast-prime? n 100)) 
  
 (define (timed-prime-test n) 
   (start-prime-test n (runtime))) 
  
 (define (start-prime-test n start-time) 
   (if (prime? n) 
       (report-prime n (- (runtime) start-time)))) 
  
 (define (report-prime n elapsed-time) 
   (newline) 
   (display n) 
   (display " *** ") 
   (display elapsed-time)) 
  
 (timed-prime-test 1009) 
 (timed-prime-test 1013) 
 (timed-prime-test 1019) 
 (timed-prime-test 10007) 
 (timed-prime-test 10009) 
 (timed-prime-test 10037) 
 (timed-prime-test 100003) 
 (timed-prime-test 100019) 
 (timed-prime-test 100043) 
 (timed-prime-test 1000003) 
 (timed-prime-test 1000033) 
 (timed-prime-test 1000037) 
  
 ; See comments in exercise 1.22 
 (newline) 
 (timed-prime-test 1000000007) 
 (timed-prime-test 1000000009) 
 (timed-prime-test 1000000021) 
 (timed-prime-test 10000000019) 
 (timed-prime-test 10000000033) 
 (timed-prime-test 10000000061) 
 (timed-prime-test 100000000003) 
 (timed-prime-test 100000000019) 
 (timed-prime-test 100000000057) 
 (timed-prime-test 1000000000039) 
 (timed-prime-test 1000000000061) 
 (timed-prime-test 1000000000063)

;ex 1.25
;The modified version of expmod computes huge intermediate results. 
;Scheme is able to handle arbitrary-precision arithmetic, but arithmetic with arbitrarily long numbers is computationally expensive. This means that we get the same (correct) results, but it takes considerably longer.


;; ex 1.26
;Instead of a linear recursion, the rewritten expmod generates a tree recursion, whose execution time grows exponentially with the depth of the tree, which is the logarithm of N. Therefore, the execution time is linear with N. 

;; ex 1.27 
  
 (define (square x) (* x x)) 
  
 (define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (remainder (square (expmod base (/ exp 2) m)) 
                     m)) 
         (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m))))         
  
 (define (full-fermat-prime? n) 
   (define (iter a n) 
     (if (= a n) true 
         (if (= (expmod a n n) a) (iter (+ a 1) n) false))) 
   (iter 1 n)) 
  
 (define (test-fermat-prime n expected) 
   (define (report-result n result expected) 
     (newline) 
     (display n) 
     (display ": ") 
     (display result) 
     (display ": ") 
     (display (if (eq? result expected) "OK" "FOOLED"))) 
   (report-result n (full-fermat-prime? n) expected)) 
  
 (test-fermat-prime 2 true) 
 (test-fermat-prime 13 true) 
 (test-fermat-prime 14 false) 
  
 (test-fermat-prime 561 false)  ; Carmichael number 
 (test-fermat-prime 1105 false) ; Carmichael number 
 (test-fermat-prime 1729 false) ; Carmichael number 
 (test-fermat-prime 2465 false) ; Carmichael number 
 (test-fermat-prime 2821 false) ; Carmichael number 
 (test-fermat-prime 6601 false) ; Carmichael number 

;; ex 1.28 
  
 (define (square x) (* x x)) 
  
 (define (miller-rabin-expmod base exp m) 
   (define (squaremod-with-check x) 
     (define (check-nontrivial-sqrt1 x square) 
       (if (and (= square 1) 
                (not (= x 1)) 
                (not (= x (- m 1)))) 
           0 
           square)) 
     (check-nontrivial-sqrt1 x (remainder (square x) m))) 
   (cond ((= exp 0) 1) 
         ((even? exp) (squaremod-with-check 
                       (miller-rabin-expmod base (/ exp 2) m))) 
         (else 
          (remainder (* base (miller-rabin-expmod base (- exp 1) m)) 
                     m)))) 
  
 (define (miller-rabin-test n) 
   (define (try-it a) 
     (define (check-it x) 
       (and (not (= x 0)) (= x 1))) 
     (check-it (miller-rabin-expmod a (- n 1) n))) 
   (try-it (+ 1 (random (- n 1))))) 
  
 (define (fast-prime? n times) 
   (cond ((= times 0) true) 
         ((miller-rabin-test n) (fast-prime? n (- times 1))) 
         (else false))) 
  
 (define (prime? n)  
    ; Perform the test how many times?  
    ; Use 100 as an arbitrary value.  
    (fast-prime? n 100))  
  
 (define (report-prime n expected)  
   (define (report-result n result expected)  
     (newline)  
     (display n)  
     (display ": ")  
     (display result)  
     (display ": ")  
     (display (if (eq? result expected) "OK" "FOOLED")))  
   (report-result n (prime? n) expected))  
    
 (report-prime 2 true)  
 (report-prime 7 true)  
 (report-prime 13 true)  
 (report-prime 15 false) 
 (report-prime 37 true)  
 (report-prime 39 false) 
   
 (report-prime 561 false)  ; Carmichael number  
 (report-prime 1105 false) ; Carmichael number  
 (report-prime 1729 false) ; Carmichael number  
 (report-prime 2465 false) ; Carmichael number  
 (report-prime 2821 false) ; Carmichael number  
 (report-prime 6601 false) ; Carmichael number 