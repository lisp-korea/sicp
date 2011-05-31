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

;; 4.1.2 Representing Expressions

;; 3,4,5
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
		((string? exp) #t)
		(else #f)))

;; a,b,c
(define (variable? exp) (symbol? exp))

;; ('quote apple),('quote banana),('quote orange)
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (Car exp) tag)
	  #f))

;; (set! <var> <exp>)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; (define <var> <val>)
;; (define (<var> <arg>) <body>)
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
	  (cadr exp)
	  (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
	  (caddr exp)
	  (make-lambeda (cdadr exp)     ;; arg
					(cddr exp))))	;; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameter body)))

;; (if <pred> <exp1> <exp2>)
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (caddr exp)))
	  (cadddr exp)
	  'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; (begin (<exp1> <exp2> ...))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; Derived expressions


;; (cond ((<pred1> <exp1>) (<pred2> <exp2>) .. (else <exp-else>)))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actinos clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
	  'false
	  (let ((first (car clauses))
			(rest (cdr clauses)))
		(if (cond-else-clause? first) ;;(else <exp-else>)
			(if (null? rest)
				(sequence->exp (cond-actinos first))
				(error "ELSE clause isn't last -- COND" clauses))
			(make-if (cond-predicate first)
					 (sequence->exp (cond-actions first))
					 (expand-clauses rest))))))

;; ex 4.2

(define (louis-eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value env))
		((quoted? exp) (text-of-qutation exp))
		((assignment? exp) (eval-assignment exp env))
		((application? exp)
		 (my-apply (my-eval (operator exp) env)
				(list-of-values (operands exp) env)))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (labmda-parameters exp)
						 (labmda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (my-eval (cond->if exp) env))
		(else
		 (error "Unknown expression type --EVAL"))))

;; a. if exp is (define x 3), louis-eval can't determine it is definition.(every special form is also APPLICATION)

;; b.

(define (louis-eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value env))
		((quoted? exp) (text-of-qutation exp))
		((assignment? exp) (eval-assignment exp env))
		((louis-application? exp)
		 (my-apply (my-eval (operator exp) env)
				(list-of-values (operands exp) env)))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (labmda-parameters exp)
						 (labmda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (my-eval (cond->if exp) env))
		(else
		 (error "Unknown expression type --EVAL"))))

(define (louis-application? exp)
  (tagged-list? exp 'call))
