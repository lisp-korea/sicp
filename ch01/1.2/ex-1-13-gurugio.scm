
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0) b
      (fib-iter (+ a b) a (- count 1))))

(define c1 (/ (+ 1 (sqrt 5)) 2))
(define c2 (/ (- 1 (sqrt 5)) 2)) 

;; fib(n) is round number of c1
(define (prove1 n)
  (= (fib n) (round (/ (expt c1 n) (sqrt 5)))))

(prove1 5)
(prove1 6)


;; If no round of value, it cannot check equality at prove2-procedure. WHY?
(define (fib-approx n)
  (round (/ (- (expt c1 n) (expt c2 n)) (sqrt 5))))

;; fib(n) = fib(n-1) + fib(n-2)
;; therefore, fib-approx(n) = fib-approx(n-1) + fib-approx(n-2)
(define (prove2 n)
  (display (fib-approx n)) (display "=?") (display (+ (fib-approx (- n 1)) (fib-approx (- n 2))))
  (if (zero? n) '#t
      (if (= (fib-approx n) (+ (fib-approx (- n 1)) (fib-approx (- n 2))))
          (begin (display "fib(") (display n) (display ") is proved") (newline)
                 (prove2 (- n 1)))
          '#f)))

(prove2 10)
(prove2 60)
(prove2 75)
(prove2 76) ;; -> return #f, WHY?
