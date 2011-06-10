
(define true #t)
(define false #f)

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; 4.1.1 언어실행기의 알짜배기

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp)
				       (lambda-body exp)
				       env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure) (eval-sequence
					  (procedure-body procedure)
					  (extend-environment
					   (procedure-parameters procedure)
					   arguments
					   (procedure-environment procedure))))
	(else
	 (error "Unknown procedure type -- APPLY" procedure))))
	  

(define (list-of-values exps env)
  (if (no-operands? exps) '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; 조건식
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
      

;; 잇단식
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

;; 덮어쓰기와 정의
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


;; 연습문제 4.1

;; --->
(define (list-of-values exps env)
  (if (no-operands? exps) '()
      (let* ((left (eval (first-operand exps) env))
	     (right (list-of-values (rest-operands exps) env)))
	(cons left right))))

;; <----
(define (list-of-values exps env)
  (if (no-operands? exps) '()
      (let ((rest (list-of-values (rest-operands epxs) env)))
	(cons (eval (first-oprand exps) env) rest))))


      
;; 4.1.2 식을 나타내는 방법

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))


(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp) (eq? (car exp) tag) false))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))


;; 덮어쓰기
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; 정의
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
		   (cddr exp))))

;; lambda

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if
(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp) 'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

;; 프로시져 적용

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))


;; 이끌어낸 식  derived expression

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses) 'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE caluses isn't last -- COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))






;; 연습문제 4.2

;; a.
;; 프로시져 적용 을 판단하는 application? 이 단지 pair 인지 아닌지만 판단하기 때문에
;; (define x 3) 같은 정의 하기 연산을 프로시져로 판단하게 된다.

;; b.
;; 그리하여 eval 에서 application? 이 먼저나온다면 exp 가 프로시져 적용임을 알리기위해
;; call 이라고 하는 tag 를 만들어 줄 수 있다.

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp)
  (cadr exp))

(define (operands exp)
  (cddr exp))


;; 연습문제 4.3

(define *table* '())

(define (get key)
  (define (iter table)
    (cond ((null? table) #f)
	  ((eq? key (caar table)) (cdar table))
	  (else (iter (cdr table)))))
  (iter *table*))

(define (put key proc)
  (define (remove table acc)
    (cond ((null? table) acc)
	  ((eq? key (caar table)) (remove (cdr table) acc))
	  (else (remove (cdr table) (append acc (list (car table)))))))
  (if (get key) (set! *table* (append (remove *table* '()) (list (cons key proc))))
      (set! *table* (append *table* (list (cons key proc))))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get (car exp))
         ((get (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL"))))

(put 'quote
     (lambda (exp env)
       (text-of-quotation exp)))

(put 'set!
     (lambda (exp env)
       (eval-assignment exp env)))

(put 'define eval-definition)

(put 'if eval-if)

(put 'lambda
     (lambda (exp env)
       (make-procedure (lambda-parameters exp)
		       (lambda-body exp)
		       env)))

(put 'begin
     (lambda (exp env)
       (eval-sequence (begin-actions exp) env)))

(put 'cond
     (lambda (exp env)
       (eval (cond->if exp) env)))

  
;; 연습문제 4.4


(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (define (eval-and-operands operands v)
    (cond ((null? operands) v)
          ((true? (eval (car operands) env))
           (eval-and-operands (cdr operands) (eval (car operands))))
          (else false)))
  (eval-and-operands (cdr exp) true))


(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (define (eval-or-operands operands)
    (cond ((null? operands) false)
          ((true? (eval (car operands) env)) true)
          (else
           (eval-or-operands (cdr operands)))))
  (eval-or-operands (cdr exp)))

;; derived expression

;; (and (= 2 2) (= 3 3))
;; (if (= 2 2) (if (= 3 3) true false) false)

(define (and? exp)
  (tagged-list? exp 'and))

(define (and-clauses exp) (cdr exp))

(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (car clauses)
               (expand-and-clauses (cdr clauses))
               'false)))

(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

;; (or (= 2 2) (= 3 2))
;; (if (= 2 2) true (if (= 3 2) true false))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (car clauses)
               'true
               (expand-or-clauses (cdr clauses)))))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))


;; 연습문제 4.5

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses) 'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error ("ELSE cluase isn't last -- COND->IF" clauses)))
            (if (tagged-list? (cond-actions first) '=>)
                (let ((proc (cadr (cond-actions first))))
                  (display (list proc (cond-predicate first)))
                  (list proc (cond-predicate first)))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))


;; 연습문제 4.6

(define (let? exp)
  (tagged-list? exp 'let))

(define (let->combination exp)
  (let ((args (cadr exp))
        (body (cddr exp)))
    (let ((vars (map car args))
          (exps (map cadr args)))
      (cons (make-lambda vars body) exps))))

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((difinition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp)
				       (lambda-body exp)
				       env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((let? exp) (eval (let->combination exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; 연습문제 4.7

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (let*->nested-lets-core args body)
    (if (null? args)
        body
        (let->combination (list 'let (list (car args))
                                (let*->nested-lets-core (cdr args) body)))))
  (let*->nested-lets-core (cadr exp) (caddr exp)))


;; 연습문제 4.8

(define (let->combination exp)
  (if (pair? (cadr exp))
      ;; normal let syntax
      (let ((args (cadr exp))
            (body (cddr exp)))
        (let ((vars (map car args))
              (exps (map cadr args)))
          (cons (make-lambda vars body) exps)))
      ;; procedure definition with let
      (let ((var (cadr exp))
            (bindings (caddr exp))
            (body (cadddr exp)))
        (let ((var-args (map car bindings))
              (exps (map cadr bindings)))
          (sequence->exp (list (cons (list 'define (cons var var-args) body) exps)
                               (cons var exps)))))))



;; 연습문제 4.9
;; -_-;;


(define (while? exp)
  (tagged-list? exp 'while))

(define (while-predicate exp)
  (cadr exp))
(define (while-body exp)
  (caddr exp))

(define (eval-while exp env)
  (display "condition:") (display (while-predicate exp)) (display " -- ")
  (display "body:") (display (while-body exp)) (newline)
  (if (true? (eval (while-predicate exp) env))
      (begin
        (eval (while-body exp) env)
        (eval exp env))))


;; 연습문제 4.10
;; -_-;;



;; 4.1.3 언어실행기에서 쓰는 데이터구조

;; 술어 검사 하기

(define true #t)
(define false #f)

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; 프로시져 표현

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))

;; 환경연산

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (enclosing-environment env)
  (cdr env))

(define (first-frame env) (car env))
(define the-empty-environment '())


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (car vals))
	    (else (scan (cdr vars)
			(cdr vals)))))
    (if (eq? env the-empty-environment) (error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))



(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (set-car! vals val))
	    (else (scan (cdr vars)
			(cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
	    ((eq? var (car vars)) (set-car! vals val))
	    (else (scan (cdr vars)
			(cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;; 연습문제 4.11
;; 환경연산

(define (make-frame variables values)
  (define (iter var val acc)
    (cond ((null? var) acc)
	  (else (iter (cdr var) (cdr val) (append acc (list (cons (car var) (car val))))))))
  (iter variables values '()))

(define (frame-variables frame)
  (define (iter frame acc)
    (cond ((null? frame) acc)
	  (else (iter (cdr frame) (append acc (list (caar frame)))))))
  (iter frame '()))

(define (frame-values frame)
  (define (iter frame acc)
    (cond ((null? frame) acc)
	  (else (iter (cdr frame) (append acc (list (cdar frame)))))))
  (iter frame '()))

(define (add-binding-to-frame! var val frame)
  (let ((old-car (car frame))
	(old-cdr (cdr frame)))
    (set-car! frame (cons var val))
    (set-cdr! frame (cons old-car old-cdr))))


(define (enclosing-environment env)
  (cdr env))

(define (first-frame env) (car env))
(define the-empty-environment '())


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-environment env)))
	    ((eq? var (caar frame)) (cdar frame))
	    (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment) (error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-environment env)))
	    ((eq? var (caar frame)) (set-cdr! (car frame) val))
	    (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((fir-frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame) (add-binding-to-frame! var val fir-frame))
	    ((eq? var (caar frame)) (set-cdr! (car frame) val))
	    (else (scan (cdr frame)))))
    (scan fir-frame)))


;; 4.1.4 언어실행기를 보통 프로그램처럼 돌려보기

;; 연습문제 4.15
(define (run-forever)
  (run-forever))

(define (try p)
  (if (halts? p p) (run-forever)
      'halted))


;; 4.1.6 안쪽정의

(define (f x)
  (define (even? n)
    (if (= n 0) true
	(odd? (- n 1))))
  (define (odd? n)
    (if (= n 0) false
	(even? (- n 1)))))


;; 너무 어렵다...........=_=
