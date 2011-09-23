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
