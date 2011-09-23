;; Chapter 5. Computing with Register Machines
;; 5.1 Designing Register Machines

;; ... --;

;; 5.1.1 A Language for Describing Register Machines

;; scheme code
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; data path
(data-paths
 (registers
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))

 ;; operation
 (operations
  ((name rem)
   (inputs (register a) (register b)))
  ((name =)
   (inputs (register b) (constant 0)))))

;; controller
(controller
 test-b
   (test =)
   (branch (label gcd-done))
   (t<-r)
   (a<-b)
   (b<-t)
   (goto (label test-b))
gcd-done)


;; unified controller
(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)

;; ex 5.2

(controller
 (assign product (const 1))
 (assign counter (const 1))
 test-counter
   (test (op >) (reg counter) (reg n))
   (branch (label factorial-done))
   (assign t (op *) (reg product) (reg counter))
   (assign product (reg t))
   (assign t (op +) (reg counter) (const 1))
   (assign counter (reg t))
   (goto (label test-counter))
 factorial-done)

;; actions

;; to print value in register a
(perform (op print) (reg a))

(controller
  gcd-loop
    (assign a (op read))
    (assign b (op read))
  test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
	(assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
  gcd-done
    (perform (op print) (reg a))
    (goto (label gcd-loop)))


;; 5.1.2 Abstraction in Machine Design

(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))

(controller
  test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (reg a))
  rem-loop
    (test (op <) (reg t) (reg b))
    (branch (label rem-done))
    (assign t (op -) (reg t) (reg b))
    (goto (label rem-loop))
  rem-done
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
  gcd-done)

;; ex 5.3

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; primitive ver.

(controller
  (assign guess (const 1.0))
  test-guess
    (test (op good-enough?) (reg guess))
    (branch sqrt-done)
    (assign t (op improve) (reg guess))
    (assign guess (reg t))
    (goto (label test-guess))
  sqrt-done)

;; expend ver.

(controller
  (assign guess (const 1.0))
  test-guess
    ;; good-enough? section ...
    (assign t (op square) (reg guess))
	(test (op <) (reg t) (const 0.001))
    (branch sqrt-done)

	;; improve section ...
	(assign t (op /) (reg x) (reg guess))
	(assign t2 (op average) (reg guess) (reg t))
	
    (assign guess (reg t2))
    (goto (label test-guess))
  sqrt-done)


;; 5.1.3 Subroutine

;; using same data path... but gcd is duplicated
gcd-1
 (test (op =) (reg b) (const 0))
 (branch (label after-gcd-1))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label gcd-1))
after-gcd-1

...

gcd-2
 (test (op =) (reg b) (const 0))
 (branch (label after-gcd-2))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label gcd-2))
after-gcd-2


;; using continue to remove duplication. but it's very restrict solution.
gcd
 (test (op =) (reg b) (const 0))
 (branch (label gcd-done))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label gcd))
gcd-done
 (test (op =) (reg continue) (const 0))
 (branch (label after-gcd-1))
 (goto (label after-gcd-2))
;; Before branching to gcd from the first place where
;; it is needed, we place 0 in the continue register
 (assign continue (const 0))
 (goto (label gcd))
after-gcd-1
;; Before the second use of gcd, we place 1 in the continue register
 (assign continue (const 1))
 (goto (label gcd))
after-gcd-2


;; save return point to continue register...
gcd
 (test (op =) (reg b) (const 0))
 (branch (label gcd-done))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label gcd))
gcd-done
 (goto (reg continue))
;; Before calling gcd, we assign to continue
;; the label to which gcd should return.
 (assign continue (label after-gcd-1))
 (goto (label gcd))
after-gcd-1
;; Here is the second call to gcd, with a different continuation.
 (assign continue (label after-gcd-2))
 (goto (label gcd))
after-gcd-2


;; 5.1.4 Using a Stack to Implement Recursion

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; it's recursive. when solve factorial n-1, must remeber n to multiply.

(controller
 (assign continue (label fact-done))
 fact-loop
   (test (op =) (reg n) (const 1)) 
   (branch (label base-case))
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))
   (goto (reg continue))
 base-case
   (assign val (const 1))
   (goto (reg continue))
 fact-done)


;; double recursion

(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)
   (assign n (op -) (reg n) (const 1))
   (goto (label fib-loop)) 
 afterfib-n-1
   (restore n)
   (restore continue)
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)
   (goto (label fib-loop))
 afterfib-n-2
   (assign n (reg val))
   (restore val)
   (restore continue)
   (assign val
		   (op +) (reg val) (reg n))
   (goto (reg continue))
 immediate-answer
   (assign val (reg n))
   (goto (reg continue))
 fib-done)


;; ex 5.4

;; a.

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(controller
 (assign continue (label expt-done))
 expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-expt))
   (goto (label expt-loop))
 after-expt
   (restore n)
   (restore continue)
   (assign (op *) (reg n) (reg b))
   (goto (reg continue))
 base-case
   (assign val (const 1))
   (goto (reg continue))
 expt-done)

;; b.

(define (expt b n)
  (define (expt-iter counter product)
	(if (= counter 0)
		product
		(expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

(controller
 (assign counter (reg n))
 (assign product (const 1))

 expt-iter-loop
   (test (op =) (reg counter) (const 0))
   (goto (label expt-done))
   (assign t (op -) (reg counter) (const 1))
   (assign counter (reg t))
   (assign t (op *) (reg b) (reg product))
   (assign product (reg t))
   (goto (label expt-iter-loop))

 expt-done)

;; ex 5.5
;; --;

;; ex 5.6


(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)
   (assign n (op -) (reg n) (const 1))
   (goto (label fib-loop)) 
 afterfib-n-1 
   (restore n) 

   ;; continue wasn't modified in this section...
   (restore continue)
   (assign n (op -) (reg n) (const 2))
   (save continue)

   
   (assign continue (label afterfib-n-2))
   (save val)
   (goto (label fib-loop))
 afterfib-n-2
   (assign n (reg val))
   (restore val)
   (restore continue)
   (assign val
		   (op +) (reg val) (reg n))
   (goto (reg continue))
 immediate-answer
   (assign val (reg n))
   (goto (reg continue))
 fib-done)

