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
(define (eval exp env)
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
	 (eval-sequence (begin-actings exp) env))
	((cond? exp) (eval (cond-if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
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

(define (apply procedure arguments)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 프로시저 인자 
;;; p478
;;; 프로시저에 건네줄 인자 리스트를 만들기 위해서 list-of-values를 쓴다.
;;; -> 모든 피연산자의 값을 구한 다음에 그 값들을 리스트로 묶어서 내놓는다.

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 조건 식 
;;; p479
;;; eval-if는 지정된 환경에서 if 식의 술어를 계산한다.

(define (eval-if exp env)
  (if (ture? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 잇단식
;;; p479
;;; apply가 프로시저 몸에서 잇단식을 만나거나
;;; eval이 begin으로 묶은 잇단식을 처리해야 할 때 eval-sequence를 쓴다.

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 덮어쓰기와 정의
;;; 덮어쓸 값을 얻어낸 다음, 그 값과 변수를 set-variable-value!로 넘겨서 지정된 환경을 고쳐쓴다.

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-variable exp) env)
    env)
  'ok)
	  



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
      (cons (eval (first-operand exps) env)
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

;; 1) 왼쪽에서 오른쪽으로
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
	    (rest (list-of-values (rest-operands exps) env)))
	(cons left rest))))

;; 2) 오른쪽에서 왼쪽으로
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env))
	    (left (eval (first-operand exps) env)))
	(cons left rest))))

