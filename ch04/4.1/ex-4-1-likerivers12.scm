
;; racket -l r5rs/run


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 4 언어를 처리하는 기법

;;; 모든 분야에서 쓰이는 기법 :
;;; * 모듈 방식 - 
;;; 기본적인 물건을 엮어서 더 복잡한 물체를 짜맞추고
;;; 그 복잡한 물체를 요약하여 더 수준 높은 부품을 만드는 것


;;; 깨달은 바 - 갖가지 생각을 제대로 표현하기 위하여 그에 맞추어 언어도 자꾸 바꿔 써야 한다.

;;; 언어 실행기도 하나의 프로그램일 뿐이다.

;;; 컴퓨터 과학 자체가 문제마다 그 풀이에 알맞은 표현 수단을 갖출 수 있도록 새 언어를 만들어 내는 분야이다.

;;; Lisp를 바타 언어로 삼아, 언어를 처리하는 Lisp 프로시저를 짜 보자


;;; Ch 4.1 메타써큘러 실행기
;;; p473

;;; 언어 실행기가 처리하려는 언어로 다시 그 실행기를 만들 때 그런 실행기를 메타써큘러 실행기라고 한다.

;;; 환경 계산법은 두 가지 규칙
;; 1. 식의 값을 구하려면, 부분 식의 값부터 모두 구해야 한다.
;; 2. 프로시저를 인자에 적용하려면 프로시저의 몸을 계산하기 위하여 새 환경부터 만든다.



;;;==========================================
;;; 4.1.1 언어 실행기의 알짜배기
;;; p475

;;; 식의 값을 구하는 프로세스는 eval 과 apply 라는 두 프로시저가 맞물려 돌아가는 것이라 볼수 있다.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eval
;;; p475
;;; : 식과 환경을 인자로 받은 다음, 식의 종류에 따라 알맞은 계산 방법을 고른다.

;; 요약된 문법
;; - 식의 종류를 알아보는 프로시저와 그 부품을 골라내는 요약된 수단


;; * 기본식(Primitive expression)
;; 1) 수 같이 바로 값을 나타내는 식을 만나면 식 자체를 돌려준다.
;; 2) 변수가 오면 환경을 뒤져본다.

;; * 특별한 형태(Special form)
;; 1) 따옴표한 식이오면 따옴표를 없앤 식을 돌려준다.
;; 2) 변수 값을 덮어쓰거나 정의하는 식이오면, 새 변수 값을 계산하기 위하여 다시 eval을 부른다.
;;    환경을 고친다.
;; 3) if 식은 참이면 결과 식을 계산하고, 그렇지 않으면 다른 식을 계산한다.
;;    특별한 과정이 따로 필요.
;; 4) lambda 식을 만나면, 인자,몸,환경을 묶어서 프로시저로 바꾸어야 한다.
;; 5) begin 식을 만나면 적어놓은 차례대로 계산한다.
;; 6) cond 식이 오면 겹쳐 쓴 if식으로 고쳐쓰고 계산한다.

;; * 엮은 식(combination)
;; 1) 프로지저 적용식을 계산하려면, eval을 여러 번 되돌리면서 그 식의
;;    연산자 부분과 피연산자 부분의 값을 모두 구해야 한다.
;;    실제 프로시저에 인자를 주고 계산하는 일은 apply가 맡아서 한다.

;;; eval 정의
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
	((application? exp)
	 (my-apply (my-eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; 위 정의는 다른 식을 처리해야할 적에 eval을 뜯어 고쳐야하는 문제가 있음.
;; -> 데이터 중심 기법


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apply
;;; p478
;;; 프로시저와 그 프로시저에 건네줄 인수 리스트를 인자로 받는다.
;;; 기본 프로시저가 오면 apply-primitive-procedure로 처리하고
;;; 합친 프로시저가 오면 그 몸이 되는 식을 차례대로 하나씩 처리한다.

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (display "<<my-apply : primitive-procedure>>")
	 (newline)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (display "<<my-apply : compound-procedure>>")
	 (newline)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error
	  "Unknown procedure type -- APPLY" procedure))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 프로시저 인자 
;;; p478
;;; 프로시저에 건네줄 인자 리스트를 만들기 위해서 list-of-values를 쓴다.
;;; -> 모든 피연산자의 값을 구한 다음에 그 값들을 리스트로 묶어서 내놓는다.

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))


(list-of-values '(1 2 3) '())
;(list-of-values '((+ 1 2) (* 1 2)) the-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 조건 식 
;;; p479
;;; eval-if는 지정된 환경에서 if 식의 술어를 계산한다.

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 잇단식
;;; p479
;;; apply가 프로시저 몸에서 잇단식을 만나거나
;;; eval이 begin으로 묶은 잇단식을 처리해야 할 때 eval-sequence를 쓴다.

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
	(else (my-eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

;;(eval-sequence (begin-actions '(begin (define a 1) (set! a 2)))
;;	       '())
;; ->
;;(eval-sequence '((define a 1) (set! a 2)) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 덮어쓰기와 정의
;;; 덮어쓸 값을 얻어낸 다음, 그 값과 변수를 set-variable-value!로 넘겨서 지정된 환경을 고쳐쓴다.

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (my-eval (assignment-value exp) env)
		       env)
  'ok)

;(assignment-variable '(set! a 1))
;(assignment-value '(set! a 1))

;in driver loop
;(set! a 1)

(define (eval-definition exp env)
  (display "eval-definition")
  (newline)
  (define-variable! (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  'ok)

;(definition-variable '(define a 1))
;(definition-value '(define a 1))	  

;in driver loop
;(define b 2)



;;;--------------------------< ex 4.1 >--------------------------
;;; p480

;; 피연산자 값을 셈하는 순서
;; 1) 왼쪽에서 오른쪽으로
;; 2) 오른쪽에서 왼쪽으로
;; - 기본적으로 바탕에 있는 Lisp에서 물려받는다.

;; 바탕 Lisp에서 물려받은 차례를 따르지 않고,
;; 1)또는 2)의 순서로 셈하도록 
;; list-of-values를 고쳐라


;; 0) 주어진 방식
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 실험
(define (f1)
  (begin 
    (display 1)
    (newline)
    1))

(define (f2)
  (begin
    (display 2)
    (newline)
    2))

(cons (f1) (f2))

(let ((a (f2)))
  (cons (f1) a))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; 1) 왼쪽에서 오른쪽으로
;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (let ((left (my-eval (first-operand exps) env))
;; 	    (rest (list-of-values (rest-operands exps) env)))
;; 	(cons left rest))))

;; ;; 2) 오른쪽에서 왼쪽으로
;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (let ((rest (list-of-values (rest-operands exps) env))
;; 	    (left (my-eval (first-operand exps) env)))
;; 	(cons left rest))))



;;;==========================================
;;; 4.1.2 식을 나타내는 방법
;;; p481

;;--------------------------------------------------------
;; * 곧바로 값을 구할 수 있는 것은 수와 글줄(문자열-string) 뿐 이다.
(define (self-evaluating? exp)
  (cond ((number? exp) #t) ;true)
	((string? exp) #t) ;true)
	(else #f))) ;false)))

;;--------------------------------------------------------
;; * 변수는 글자로 나타낸다.
(define (variable? exp) (symbol? exp))

;;--------------------------------------------------------
;; * 따온 식은 (quote <text-of-quotation>) 꼴로 나타낸다.
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;;--------------------------------------------------------
;; * 덮어쓰기는 (set! <var> <value>)꼴로 나타낸다.
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;--------------------------------------------------------
;; * 정의는 다음 두 꼬러 가운데 하나다.
;; - (define <var> <value>)
;; - (define (<var> <parameter1> ... <parameter_n>) 
;;     <body>)
;;   위는 아래 식의 달콤한 문법이다.
;;   (define <var>
;;      (lambda (<parameter1> ... <parameter_n>)
;;        <body>))
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)           ; 1) (define a 1)
      (caadr exp)))        ; 2) (define (f x) ... )

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)  ; 매개변수 이름
		   (cddr exp))))

;;--------------------------------------------------------
;; * lambda 식은 lambda 글자로 시작하는 리스트다.
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; in driver loop
;(lambda (x) (+ x 1))

;;--------------------------------------------------------
;; * 조건식은 if로 시작하여 술어, 결과 식, 다른 결과 식으로 구성된다.
;;   다른 결과 식이 없으면 그 값은 언제나 false
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      '#f)) ;'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;--------------------------------------------------------
;; * begin은 잇단식을 하나로 묶어낸다.
;; - 첫 식과 나머지 모든 식을 골라내는 고르개
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

;; sequence->exp 짜맞추개
(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;(begin? '(begin (define a 1) (set! a 2)))
;(begin-actions '(begin (define a 1) (set! a 2)))
;(sequence->exp '((define a 1) (set! a 2)))

;; * 프로시적 적용은 또 다른 합친 식이다.
;;   식의 car는 연산자, cdr는 피연산자
(define (application? exp) (pair? exp))
;; exp :=
;; ( (lambda (<parameters...>) <body>)
;;   (<arguments ...>) )


(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;; in driver-loop
;((lambda (x) (+ x 1)) 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 이끌어낸 식 (derived expression)
;;; p485

;; 따로 프로시저를 만들어 처리하지 않아도 다른 특별한 형태의 힘을 빌려 처리할 수 있는 것
;; : 계산 방법을 따로 만들어 주어야 할 특별한 형태가 줄어들기 때문에,
;;   실행기가 단순해진다.
;; (cond ((> x 0) x)
;;       ((= x 0) (display 'zero) 0)
;;       (else (- x)))
;; ;;=>
;; (if (> x 0)
;;     x
;;     (if (= x 0)
;; 	(begin (display 'zero)
;; 	       0)
;; 	(- x)))

;; cond 에 대한 ..

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (display "cond->if")
  (newline)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      '#f ; 'false    ; else 절이 없을 경우
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF"
		       clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

;; in driver-loop
;; (cond (true (+ 1 2) (* 1 2))
;;       (else 4))
;; (cond (false (+ 1 2) (* 1 2))
;;       (else 4))


;;;--------------------------< ex 4.2 >--------------------------
;;; p487

;; eval 프로시저의 cond절에서 
;;  덮어쓰기 연산, 정의보다
;;  프로시저 적용이 먼저오게 나오게 하면?

;; a) 잘못된 점은?
;;  exp : (define x 3)을 처리할 때
;; 1) (application? exp) 에서 pair? -> ture가 되어서 apply로 넘어감
;; 2) -> operator : define
;;    (eval 'define env) ...
;; 

;; b) 프로시저 적용식이 언제나 call로 시작되게 하면?
;; (factorial 3) -> (call factorial 3)
;; (+ 1 2) -> (call + 1 2)

;; 그러면 기존에 정의한 프로시저를 조금 바꿔서 적용 가능할 듯...



;;;--------------------------< ex 4.3 >--------------------------
;;; p488

;; 기존 eval 테스트
(my-eval 1 '())

;; (my-eval '(define a 1) '())

;;;-----------------------------------------------
;;; put, get 구현
;; 별파란님 구현
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
  (if (get key) 
      (set! *table* (append (remove *table* '()) (list (cons key proc))))
      (set! *table* (append *table* (list (cons key proc))))))
;;;-----------------------------------------------

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get (car exp))
         ((get (car exp)) exp env))
        ((application? exp)
         (my-apply (my-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL"))))

(put 'quote
     (lambda (exp env)
       (text-of-quotation exp)))

(put 'set!
     (lambda (exp env)
       (eval-assignment exp env)))

(put 'define
     (lambda (exp env) 
       (eval-definition exp env)))

(put 'if
     (lambda (exp env)
       (eval-if exp env)))

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
       (my-eval (cond->if exp) env)))

;;;-----------------------------------------------
;;; put, get 구현
;; (define SFT (make-hash))

;; (define (put op types behavior)
;;   (hash-set! SFT (cons op types) behavior))
;; ;;  (hash-set! GOP (cons 'key (cons op types)) behavior))

;; (define (get op types)
;;   (hash-ref SFT (cons op types)))
;; ;;  (hash-ref GOP (cons 'key (cons op types))))
;; ;;;---


;; ;;; eval 정의
;; (define (my-eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;; 	((variable? exp) (lookup-variable-value exp env))
;; 	((quoted? exp) (text-of-quotation exp))
;; 	((assignment? exp) (eval-assignment exp env))
;; 	((definition? exp) (eval-definition exp env))
;; 	((if? exp) (eval-if exp env))
;; 	((lambda? exp)
;; 	 (make-procedure (lambda-parameters exp)
;; 			 (lambda-body exp)
;; 			 env))
;; 	((begin? exp)
;; 	 (eval-sequence (begin-actions exp) env))
;; 	((cond? exp) (my-eval (cond->if exp) env))
;; 	((application? exp)
;; 	 (my-apply (eval (operator exp) env)
;; 		(list-of-values (operands exp) env)))
;; 	(else
;; 	 (error "Unknown expression type -- EVAL" exp))))
;; =>

;; ;;; eval 정의
;; (define (my-eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;; 	((variable? exp) (lookup-variable-value exp env))

;; 	((application? exp)
;; 	 (my-apply (eval (operator exp) env)
;; 		(list-of-values (operands exp) env)))
;; 	(else
;; 	 (error "Unknown expression type -- EVAL" exp))))

;; (define (install-my-eval1-package)
;;   ;; 갇힌 프로시저

;;   ;;----------------------------------------------
;;   ;; * 따온 식- 'quote
;;   (define (quoted? exp)
;;     (tagged-list? exp 'quote))

;;   (define (text-of-quotation exp) (cadr exp))

;;   (define (tagged-list? exp tag)
;;     (if (pair? exp)
;; 	(eq? (car exp) tag)
;; 	#f))

;;   ;;----------------------------------------------
;;   ;; * 덮어쓰기 - 'set!
;;   (define (assignment? exp)
;;     (tagged-list? exp 'set!))

;;   (define (assignment-variable exp) (cadr exp))

;;   (define (assignment-value exp) (caddr exp))

;;   ;; * 정의 - 'define
;;   (define (definition? exp)
;;     (tagged-list? exp 'define))

;;   (define (definition-variable exp)
;;     (if (symbol? (cadr exp))
;; 	(cadr exp)           ; 1) (define a 1)
;; 	(caadr exp)))        ; 2) (define (f x) ... )

;;   (define (definition-value exp)
;;     (if (symbol? (cadr exp))
;; 	(caddr exp)
;; 	(make-lambda (cdadr exp)  ; 매개변수 이름
;; 		     (cddr exp))))

;;   ;;----------------------------------------------
;;   ;; * 'lambda 
;;   (define (lambda? exp) (tagged-list? exp 'lambda))

;;   (define (lambda-parameters exp) (cadr exp))

;;   (define (lambda-body exp) (cddr exp))

;;   (define (make-lambda parameters body)
;;     (cons 'lambda (cons parameters body)))

;;   ;;----------------------------------------------
;;   ;; * 'if
;;   (define (if? exp) (tagged-list? exp 'if))

;;   (define (if-predicate exp) (cadr exp))

;;   (define (if-consequent exp) (caddr exp))

;;   (define (if-alternative exp)
;;     (if (not (null? (cdddr exp)))
;; 	(cadddr exp)
;; 	'#f)) ;'false))

;;   (define (make-if predicate consequent alternative)
;;     (list 'if predicate consequent alternative))

;;   ;;----------------------------------------------
;;   ;; * 'begin
;;   (define (begin? exp) (tagged-list? exp 'begin))

;;   (define (begin-actions exp) (cdr exp))

;;   (define (last-exp? seq) (null? (cdr seq)))

;;   (define (first-exp seq) (car seq))

;;   (define (rest-exps seq) (cdr seq))

;;   ;; sequence->exp 짜맞추개
;;   (define (sequence->exp seq)
;;     (cond ((null? seq) seq)
;; 	  ((last-exp? seq) (first-exp seq))
;; 	  (else (make-begin seq))))

;;   (define (make-begin seq) (cons 'begin seq))
;;   ;;----------------------------------------------

;;   ;; 이 꾸러미의 인터페이스
;;   (define (tag x) (attach-tag 'rectangular x))
;;   (put 'quote 'my-eval1 text-of-quotation)
;;   (put 'set! 'my-eval1 eval-assignment)
;;   (put 'define 'my-eval1 eval-definition)
;;   (put 'if 'my-eval1 eval-if)
;;   (put 'lambda 'my-eval1 make-procedure)
;;   (put 'begin 'my-eval1 eval-sequence)
;;   (put 'cond 'my-eval1 my-eval)  ;; <<--

;;   'done)



;;;--------------------------< ex 4.4 >--------------------------
;;; p488

;; and, or 정의하기

;; a) 특별한 형태로 정의
;; (and a b c)
(define (and? exp) (tagged-list? exp 'and))

(define (rest-predicates exp) (cdr exp))

(define (eval-and exp env)
  (define (eval-and-inner predicates env2)
    (let ((a-predicates (car predicates)))
      (let ((a-res (my-eval a-predicates env2)))
	(if a-res
	    (if (null? (cdr predicates))
		a-res
		(eval-and-inner (rest-predicates predicates) env2))
	    #f))))
  (eval-and-inner (rest-predicates exp) env))


(define (eval-or exp env)
  (define (eval-or-inner predicates env2)
    (let ((a-predicates (car predicates)))
      (let ((a-res (my-eval a-predicates env2)))
	(if a-res
	    a-res
	    (if (null? (cdr predicates))
		a-res
		(eval-or-inner (rest-predicates predicates) env2))))))
  (eval-or-inner (rest-predicates exp) env))


(put 'and
     (lambda (exp env)
       (eval-and exp env)))

; in driver loop
;(and 1 2 3)
;(and true 2 false)

(put 'or
     (lambda (exp env)
       (eval-or exp env)))

; in driver loop
;(or 1 2 3)
;(or true 2 3)

;; b) 이끌어낸 식으로 정의
;; cond를 expand할 때는 표현의 문법 구조만 보고 그대로 확장한다
;; and/or를 확장할 때는 표현 각각의 값을 평가한 다음에 결과에 따라서 확장한다.



;;;--------------------------< ex 4.5 >--------------------------
;;; p489

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

;;;--------------------------< ex 4.6 >--------------------------
;;; p489



;;;--------------------------< ex 4.7 >--------------------------
;;; p489



;;;--------------------------< ex 4.8 >--------------------------
;;; p490



;;;--------------------------< ex 4.9 >--------------------------
;;; p491



;;;--------------------------< ex 4.10 >--------------------------
;;; p491





;;;==========================================
;;; 4.1.3 언어 실행기에서 쓰는 데이터 구조
;;; p491

;;; 겉으로 드러나는 식의 문법 뿐 아니라
;;; 실행기 프로그램이 돌아가는 데 꼭 필요한 데이터 구조를 정의해야 한다.
;;; ( 프로시저, 환경의 표현 방법, 참이나 거짓 값의 표현 방법 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 술어 검사하기
;;; false 아닌 값이 오면 모두 참이라고 본다.
(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 프로시저 표현
;;; 언어의 기본 원소를 처리할 때, 다음과 같은 프로시저를 쓸 수 있다고 가정한다.

;; * (apply-primitive-procedure <proc> <args>
;; : <proc>에 <arg>리스트에 들어 있는 인자 값을 건네주고 그 값을 구할 때 쓴다.

;; * (primitive-procedure? <proc>
;; : <proc>이 기본 프로시저인지 아닌지 물어보는데 쓴다.

;; 인자, 프로시저 몸, 환경을 받아 새로 합친 프로시저를 만드는 짜맞추개
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 환경 연산
;;; 환경 : 여러 변수 일람표를 한 줄로 이어놓은 것
;;; 변수 일람표 : 변수와 값을 한 쌍으로 묶어둔 표

;; * (look-variable-value <var> <env>
;; : <env>를 뒤져서 <var>와 짝을 이루는 값을 찾아서 내놓는다.

;; * (extend-environment <variables> <values> <base-env>)
;; : 새 환경을 내놓는다.

;; * (define-variable! <var> <value> <env>)
;; : <env>환경의 첫번째 일람표에 <var> <value> 정의 집어넣기

;; * (set-variable-value! <var> <value> <env>)
;; : 값 고치기

;;; 환경을 변수 일람표들의 리스트로 표현
;; (cons '((c) (3)) (cons '((b) (2)) '((a) (1)))))
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;; 변수 일람표는 두 리스트의 쌍으로 나타낸다.
;;; ((var1 var2 ...) val1 val2 ...)
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;;------
(make-frame 'a 1)  ; '(a . 1)

(define frame1 (make-frame '(a b c) '(1 2 3)))
frame1
 ; '((a b c) 1 2 3)

(add-binding-to-frame! 'd 4 frame1)
frame1
;((d a b c) 4 1 2 3)
;;------


;; 환경 넓히기
;; - 변수 리스트와 값 리스트로 새 변수 일람표를 만든 다음에 이를 환경에 이어 붙임
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

;;------
(define env1 (extend-environment '(a b) '(1 2) the-empty-environment))
env1
;; '(((a b) 1 2))

(define env2 (extend-environment '(c d) '(3 4) env1))
env2
;; '(((c d) 3 4) ((a b) 1 2))

(enclosing-environment env2)
;; '(((a b) 1 2))
;;------

;; 지정된 환경에서 변수를 찾을 때
;; - 첫 번째 일람표의 변수 리스트...
;;   없으면 그 일람표를 둘러싼 환경
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
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(lookup-variable-value 'a env2)


;; 정해진 환경에서 어떤 변수 값을 고쳐쓸 때
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
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;;;-----------

env2
(lookup-variable-value 'a env2)

(set-variable-value! 'a 5 env2)
env2
(lookup-variable-value 'a env2)

(set-variable-value! 'a 1 env2)
env2
(lookup-variable-value 'a env2)
;;;-----------


;; 변수를 정의할 때
(define (define-variable! var val env)
  (display "define-varialbe!")
  (newline)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

env2 ;(((c d) 3 4) ((a b) 1 2))
(define-variable! 'e 5 env2)
env2 ;(((e c d) 5 3 4) ((a b) 1 2))

;;;--------------------------< ex 4.11 >--------------------------
;;; p496



;;;--------------------------< ex 4.12 >--------------------------
;;; p497



;;;--------------------------< ex 4.13 >--------------------------
;;; p497





;;;==========================================
;;; 4.1.4 언어 실행기를 보통 프로그램처럼 돌려보기
;;; p497

;; 언어 실행기가 있음으로 하여, Lisp 식의 계산 프로세스를 설명하는 프로그램 하나를 손에 쥐게 되었다.

;; 남은 일은 기본 프로시저가 돌아가도록 밑바탕에 있는 Lisp 시스템에서 힘을 빌려 쓰는 것

;; 각 기본 프로시저 미리 환경
(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))


(define (primitive-procedure-names)
  (map car
       primitive-procedures))


(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
     

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	;;<추가되는 기본 원소들>
;;	(list 'define define)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	))


(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;;----------
;; (define (apply-in-underlying-scheme proc args)
;;   (apply proc args))
;;(define apply-in-underlying-scheme my-apply)
(define apply-in-underlying-scheme apply)

;;-----------

;;;---------------------
;;; 드라이버 루프 만들기
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
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))


(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))


;; 드라이버 루프 시작
(define the-global-environment (setup-environment))

;;(driver-loop)


;;;--------------------------< ex 4.14 >--------------------------
;;; p501





;;;==========================================
;;; 4.1.5 프로그램도 데이터처럼
;;; p501

;; 언어 실행기는 만능기계이다(universal machine)
;; 언어 실행기는 프로그래밍 언어로 처리하는 데이터 물체와,
;;             프로그래밍 언어 자체를 이어주는 다리 구실을 한다.

;; 대부분의 lisp 구현은 식과 환경을 인자로 받아 식의 값을 셈할 수 있도록
;; eval을 기본 프로시저로 내놓는다.
;;(eval '(* 5 5) user-initial-environment)
;;(eval (cons '* (list 5 5)) user-initial-environment)



;;;--------------------------< ex 4.15 >--------------------------
;;; p505

;; halting problem 이 unsolvable 임을 밝혀라.




;;;==========================================
;;; 4.1.6 안쪽 정의
;;; p506



;;;--------------------------< ex 4.16 >--------------------------
;;; p508


;;;--------------------------< ex 4.17 >--------------------------
;;; p509


;;;--------------------------< ex 4.18 >--------------------------
;;; p509

;;;--------------------------< ex 4.19 >--------------------------
;;; p510

;;;--------------------------< ex 4.20 >--------------------------
;;; p511

;;;--------------------------< ex 4.21 >--------------------------
;;; p512



;;;==========================================
;;; 4.1.7 문법 분석과 실행 과정을 떼어놓기
;;; p514



;;;--------------------------< ex 4.22 >--------------------------
;;; p519


;;;--------------------------< ex 4.23 >--------------------------
;;; p519


;;;--------------------------< ex 4.24 >--------------------------
;;; p520

