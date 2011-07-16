(define (error reason . args)
      (display "Error: ")
      (display reason)
      (for-each (lambda (arg) 
                  (display " ")
		  (write arg))
		args)
      (newline)
      (scheme-report-environment 5))
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


;; ex 4.26

;; Ben's side

(define (unless? exp)
  (tagged-list? exp 'unless))
(define (unless-predicate exp)
  (cadr exp))
(define (unless-consequent exp)
  (if (not (null? (cdddr exp)))
	  (cadddr exp)
	  'false))
(define (unless-alternative exp)
  (caddr exp))
(define (unless->if exp)
  (make-if
   (unless-predicate exp)
   (unless-consequent exp)
   (unless-alternative exp)))

;; Alyssa's side
;; it is possible only if evaluator supports lazy evaluation.
(define (unless pred consequent alternative)
  (if (not pred) consequent alternative))

(map if '(true false true) '(1 2 3) '(4 5 6))

;; 4.2.2 An Interpreter with Lazy Evaluation
;; from 4.1

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (my-eval (cond->if exp) env))
		((let? exp) (my-eval (let->combination exp) env))
		((application? exp)
		 (my-apply (my-eval (operator exp) env)
				   (list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type --EVAL" exp))))

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
			(list-of-values (rest-operands exps) env))))

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
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (car exp) tag)
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
	  (make-lambda (cdadr exp)     ;; arg
				   (cddr exp))))	;; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; (if <pred> <exp1> <exp2>)
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (caddr exp)))
	  (cadddr exp)
	  #f))
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
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
	  #f
	  (let ((first (car clauses))
			(rest (cdr clauses)))
		(if (cond-else-clause? first) ;;(else <exp-else>)
			(if (null? rest)
				(sequence->exp (cond-actions first))
				(error "ELSE clause isn't last -- COND" clauses))
			(make-if (cond-predicate first)
					 (sequence->exp (cond-actions first))
					 (expand-clauses rest))))))


(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))



(define (let? exp)
  (tagged-list? exp 'let))

;; (let ((<var> <exp>) ... (<var> <exp>))
;;    <body>)
;; =>
;; ((lambda (<var> ... <var>)
;;     <body>)
;;   <exp> ... <exp>)

(define (let->combination exp)
  (let ((bindings (cadr exp))
		(body (cddr exp)))
	(cons (make-lambda (map car bindings) body)
		  (map cadr bindings))))

(define (make-let bindings body)
  (list 'let bindings body))
(define (let*->nested-lets exp)
  (let ((body (caddr exp)))
	(define (make bindings)
	  (if (null? bindings) body
		  (make-let (list (car bindings))
					(make (cdr bindings)))))
	(make (cadr exp))))

;; Representing procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Operations on Environments

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
	  (cons (make-frame vars vals) base-env)
	  (if (< (length vars) (length vals))
		  (error "Too many arguments supplied" vars vals)
		  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars)
			 (env-loop (enclosing-environment env)))
			((eq? var (car vars))
			 (car vals))
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
		  (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars)
			 (env-loop (enclosing-environment env)))
			((eq? var (car vars))
			 (set-car! vals val))
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable -- SET!" var)
		(let ((frame (first-frame env)))
		  (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
	(define (scan vars vals)
	  (cond ((null? vars)
			 (add-binding-to-frame! var val frame))
			((eq? var (car vars))
			 (set-car! vals val))
			(else (scan (cdr vars) (cdr vals)))))
	(scan (frame-variables frame) (frame-values frame))))



(define (setup-environment)
  (let ((initial-env
		 (extend-environment (primitive-procedure-names)
							 (primitive-procedure-objects)
							 the-empty-environment)))
	(define-variable! 'true #t initial-env)
	(define-variable! 'false #f initial-env)
	initial-env))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
		(list 'cdr cdr)
		(list 'cons cons)
		(list 'null? null?)
		(list 'display display)
		(list 'newline newline)
		(list '+ +)
		(list '* *)
		(list '/ /)
		(list '- -)
		(list '= =)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
	   primitive-procedures))
(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
(define apply-in-underlying-scheme apply)

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
	(let ((output (my-eval input the-global-environment)))
	  (announce-output output-prompt)
	  (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))
(define (announce-output string)
  (newline)
  (display string)
  (newline))

 (define (user-print object)
  (if (compound-procedure? object)
	  (display (list 'compound-procedure
					 (procedure-parameters object)
					 (procedure-body object)
					 '<procedure-env>))
	  (display object)))

(define the-global-environment (setup-environment))


;; Modifying the evaluator


(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (my-eval (cond->if exp) env))
		((let? exp) (my-eval (let->combination exp) env))
		((application? exp)
		 (my-apply (actual-value (operator exp) env)
				   (operands exp)
				   env))
		(else
		 (error "Unknown expression type --EVAL" exp))))

(define (actual-value exp env)
  (force-it (my-eval exp env)))

(define (my-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
		 (apply-primitive-procedure
		  procedure
		  (list-of-arg-values arguments env))) ;; changed

		((compound-procedure? procedure)
		 (eval-sequence
		  (procedure-body procedure)
		  (extend-environment
		   (procedure-parameters procedure)
		   (list-of-delayed-args arguments env) ;; changed
		   (procedure-environment procedure))))

		(else
		 (error
		  "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
	  '()
	  (cons (actual-value (first-operand exps) env)
			(list-of-arg-values (rest-operands exps)
								env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
	  '()
	  (cons (delay-it (first-operand exps) env)
			(list-of-delayed-args (rest-operands exps)
								  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
	  (my-eval (if-consequent exp) env)
	  (my-eval (if-alternative exp) env)))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
	(let ((output
		   (actual-value input the-global-environment)))
	  (announce-output output-prompt)
	  (user-print output)))
  (driver-loop))

;; Representing thunks

(define (force-it obj)
  (if (thunk? obj)
	  (actual-value (thunk-exp obj) (thunk-env obj))
	  obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk)
  (cadr thunk))

(define (thunk-env thunk)
  (caddr thunk))

;; memoized ver.
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
		 (let ((result (actual-value
						(thunk-exp obj)
						(thunk-env obj))))
		   (set-car! obj 'evaluated-thunk)
		   (set-car! (cdr obj) result)
		   (set-cdr! (cdr obj) '())
		   result))
		 ((evaluated-thunk? obj)
		  (thunk-value obj))
		 (else obj)))


;; ex 4.27
(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define w (id (id 10)))
;;; L-Eval input:
count
;; force-it (id (id 10))
;;; L-Eval value: 1
;;; L-Eval input:
w
;; force-it (id (id 10))
;; force-it (id 10)
;;; L-Eval value: 10
;;; L-Eval input:
count
;;; L-Eval value: 2

;; ex 4.28

;; if eval doesn't use force-it...
(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (my-eval (cond->if exp) env))
		((let? exp) (my-eval (let->combination exp) env))
		((application? exp)
		 (my-apply (my-eval (operator exp) env)
				   (operands exp)
				   env))
		(else
		 (error "Unknown expression type --EVAL" exp))))


(my-eval '((lambda (f)
			 (f 1 2)) +) the-global-environment)

;; error occured (f is thunk.)


(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (my-eval (cond->if exp) env))
		((let? exp) (my-eval (let->combination exp) env))
		((application? exp)
		 (my-apply (actual-value (operator exp) env)
				   (operands exp)
				   env))
		(else
		 (error "Unknown expression type --EVAL" exp))))

(my-eval '((lambda (f)
			 (f 1 2)) +) the-global-environment)
;;3


;; ex 4.29

((lambda (n)
   ((lambda (fibo)
	  (fibo fibo n))
	(lambda (fb n)
	  (cond ((= n 0) 1)
			((= n 1) 1)
			(else (+ (fb fb (- n 1))
					 (fb fb (- n 2))))))))
 10)

;; fb is always evaluated twice.

;; in applicative order
(define (square x)
  (* x x))
(square (id 10))
;;100

count
;;1

;; in normal order (memoized ver.)
(define (square x)
  (* x x))
(square (id 10))
;;100

count
;;1

;; in normal order (non-memoized ver.)
(define (force-it obj)
  (if (thunk? obj)
	  (actual-value (thunk-exp obj) (thunk-env obj))
	  obj))
(define (square x)
  (* x x))
(square (id 10))
;;100

count
;;2


;; ex 4.30

;; a.

(define (for-each proc items)
  (if (null? items)
	  'done
	  (begin (proc (car items))
			 (for-each proc (cdr items)))))

(for-each (lambda (x)
				  (newline)
				  (display x))
		  '(57 321 88))

;; (if pred cons alter) is one statement. (begin (proc) (for-each)) hasn't side effect...


;; b.

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
	e
	x)
  (p (set! x (cons x '(2)))))

(p1 1)
;; (1 2)

(p2 1)
;; 1

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
		 (my-eval (first-exp exps) env))
		(else
		 (actual-value (first-exp exps) env)
		 (eval-sequence (rest-exps exps) env))))

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (my-eval (cond->if exp) env))
		((let? exp) (my-eval (let->combination exp) env))
		((application? exp)
		 (my-apply (actual-value (operator exp) env)
				   (operands exp)
				   env))
		(else
		 (error "Unknown expression type --EVAL" exp))))

(define (my-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
		 (apply-primitive-procedure
		  procedure
		  (list-of-arg-values arguments env))) ;; changed

		((compound-procedure? procedure)
		 (eval-sequence
		  (procedure-body procedure)
		  (extend-environment
		   (procedure-parameters procedure)
		   (list-of-delayed-args arguments env) ;; changed
		   (procedure-environment procedure))))

		(else
		 (error
		  "Unknown procedure type -- APPLY" procedure))))

(p1 1)
;; (1 2)

(p2 1)
;; (1 2)


;; c

(define (for-each proc items)
  (if (null? items)
	  'done
	  (begin (proc (car items))
			 (for-each proc (cdr items)))))

(for-each (lambda (x)
				  (newline)
				  (display x))
		  '(57 321 88))

;; (if pred cons alter) is one statement. (begin (proc) (for-each)) hasn't side effect...

;; d


;; ex 4.31
;; (define (f a (b lazy) c (d lazy-memo)))

(define (procedure-parameters p)
  (let ((params (cadr p)))
	(map (lambda (param)
		   (if (pair? param)
			   (car param)
			   param)) params)))

(define (list-of-args exps params env)
  (if (no-operands? exps)
	  '()
	  (let ((first-p (car params))
			(first-op (first-operand exps)))
		(let ((value 
			   (cond ((not (pair? first-p)) (actual-value first-op env))
					 ((eq? 'lazy (cadr first-p))
					  (delay-it first-op env))
					 ((eq? 'lazy-memo (cadr first-p))
					  (memoizied-delay-it first-op env)))))
		  (cons value 
				(list-of-args (rest-operands exps)
							  (cdr params)
							  env))))))
		  
	

(define (my-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
		 (apply-primitive-procedure
		  procedure
		  (list-of-arg-values arguments env)))

		((compound-procedure? procedure)
		 (eval-sequence
		  (procedure-body procedure)
		  (extend-environment
		   (procedure-parameters procedure)
		   (list-of-args arguments (cadr procedure) env)
		   (procedure-environment procedure))))

		(else
		 (error
		  "Unknown procedure type -- APPLY" procedure))))

(define (memoizied-delay-it exp env)
  (list 'memoizied-thunk exp env))

(define (memo-thunk? obj)
  (tagged-list? obj 'memoizied-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
		 (actual-value (thunk-exp obj) (thunk-env obj)))
		((memo-thunk? obj)
		 (let ((result (actual-value
						(thunk-exp obj)
						(thunk-env obj))))
		   (set-car! obj 'evaluated-thunk)
		   (set-car! (cdr obj) result)
		   (set-cdr! (cdr obj) '())
		   result))
		 ((evaluated-thunk? obj)
		  (thunk-value obj))
		 (else obj)))

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (my-eval (cond->if exp) env))
		((let? exp) (my-eval (let->combination exp) env))
		((application? exp)
		 (my-apply (actual-value (operator exp) env)
				   (operands exp)
				   env))
		(else
		 (error "Unknown expression type --EVAL" exp))))

(my-eval '(define (f a (b lazy) c (d lazy-memo))
			(if (= a 0)
				(/ b 0)
				(if (= c 1)
					(/ d 0)
					(display "success"))))
		 the-global-environment)

(my-eval '(f 1 3 0 10) the-global-environment)

;; 4.2.3 Streams as Lazy lists

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

(define (list-ref items n)
  (if (= n 0)
	  (car items)
	  (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items) '()
	  (cons (proc (car items)) (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
		((null? list2) list1)
		(else
		 (cons (+ (car list1) (car list2))
			   (add-lists (cdr list1) (cdr list2))))))
(define ones (cons 1 (cons 1 ones)))
(define integers (cons 1 (add-lists ones integers)))

(define (integral integrand initial-value dt)
  (define int
	(cons initial-value (add-lists
						 (scale-list integrand dt) int)))
  int)
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y)) y)

;; ex 4.32

;; streams

(define (stream-car stream) (car stream)) ;; car
(define (stream-cdr stream) (force (cdr stream))) ;; cdr (with squeeze)

;; using macro (arguemnt x, y aren't evaluated.)
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y))))) ;; delay only y.

(define test-pair (cons-stream x y)) ;; error occured because x is evaluated immediately.

(define test-pair (cons-stream 10 y))

(define z (cons-stream
		   (stream-cdr test-pair)
		   (stream-car test-pair))) ;; error occured because y is evaluated calling (stream-cdr)

	  
;; *lazier* lazy list

(driver-loop)

(define test-pair (cons x y))

(define z (cons (cdr test-pair) (car test-pair)))

(define x 10)

(define y 20)

(car z)

(cdr z)


;; ex 4.33


(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (value-of-quotation exp env))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (my-eval (cond->if exp) env))
		((let? exp) (my-eval (let->combination exp) env))
		((application? exp)
		 (my-apply (actual-value (operator exp) env)
				   (operands exp)
				   env))
		(else
		 (error "Unknown expression type --EVAL" exp))))

(define (value-of-quotation exp env)
  (let ((value (cadr exp)))
	(if (pair? value)
		(my-eval (quoted-exp->list value) env)
		value)))

(define (quoted-exp->list exp)
  (if (null? exp)
	  ''()
	  (let ((value (car exp)))
		(list 'cons (list 'quote value)
			  (quoted-exp->list (cdr exp))))))

;; ex 4.34

