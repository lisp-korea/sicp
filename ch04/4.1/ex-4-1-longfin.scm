(define (error reason . args)
      (display "Error: ")
      (display reason)
      (for-each (lambda (arg) 
                  (display " ")
		  (write arg))
		args)
      (newline)
      (scheme-report-environment -1))
;; 4. Metalinguistic Abstraction


;; 4.1 The Metacircular Evaluator
;; 1. To evaluate a combination(a compound expresssion other than a special form), *evaluate the subexpressi;;    ons* and then *apply the value of operator subexpression to the values of the operand subexpression*.
;; 2. To apply a compound procedure to a set of argumnets, evaluate the body of the procedure in a new envir;;    oment. To construct this environment, extend the enviornment part of the procedure object by a frame i;;    n which the formal parameters of the procedure are bound to the arguments to which the procedure is ap;;    plied.

;; 4.1.1 The Core of the Evaluator


;; Eval

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value env))
		((quoted? exp) (text-of-qutation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (labmda-parameters exp)
						 (labmda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (my-eval (cond->if exp) env))
		((application? exp)
		 (my-apply (my-eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type --EVAL"))))

;; Apply

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
		 (apply-primitive-procedure procedure arguments))
		((compound-procedure? procedure)
		 (eval-sequence
		  (procedure-body procedure)
		  (extend-environment
		   (procedure-parameters procedure)
		   arguments
		   (procedure-environment procedure))))
		(else
		 (error
		  "Unknown procedure type -- APPLY" procedure))))


;; Procedure arguments

(define (list-of-values exps env)
  (if (no-operands? exps)
	  '()
	  (cons (my-eval (first-operand exps) env)
			(list-of-values (rest-operands exps)))))

;; Conditionals

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
	  (my-eval (if-consequent exp) env)
	  (my-eval (if-alternative exp) env)))

;; Sequences

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
		(else (my-eval (first-exp exps) env)
			  (eval-sequence (rest-exps exps) env))))

;; Assignments and definitions

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
					   (my-eval (assignment-value exp) env)
					   env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
	(my-eval (definition-value exp) env)
	env)
  'ok)

;; ex 4.1

;;ltr version
(define (list-of-values-ltr exps env)
  (if (no-operands? exps)
	  '()
	  (let ((first (my-eval (first-operand exps) env))
			(rest (list-of-values (rest-operands exps))))
		(cons first rest))))

;;rtl version
(define (list-of-values-rtl exps env)
  (if (no-operands? exps)
	  '()
	  (let ((rest (list-of-values (rest-operands exps)))
			(first (my-eval (first-operand exps) env)))
		(cons first rest))))

