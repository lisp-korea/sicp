;; 4.2 Variatinos on a Scheme -- Lazy Evaluation

;; 4.2.1 Normal Order and Applicative Order

(define (try a b)
  (if (= a 0) 1 b))

(try 0 (/ 1 0)) ;;it occurs error

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

;; ex 4.25

(define (factorial n)
  (unless (= n 1)
		  (* n (factorial (- n 1)))
		  1))


;; it goes to infinite loop because in applicative order, (unless)'s arguemnts((= n 1), (* n (factorial (- n 1))) and 1) are evaluated before check predicate.

;; if scheme uses normal order, it works normally.
