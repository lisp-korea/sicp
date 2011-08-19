
;;; 4.2 Scheme 바꿔보기 - 제때 계산법(lazy evaluation)

;;;========================================
;;; 4.2.1 식의 값을 구하는 차례 - 정의대로 계산법과 인자 먼저 계산법






;;;--------------------------< ex 4.25 >--------------------------


;;;--------------------------< ex 4.26 >--------------------------



;;;========================================
;;; 4.2.2 제때 계산법을 따르는 실행기


;;; 값을 구해야할 인자와 밀쳐놓을 인자를 판별해야한다.
;;; 밀쳐놓을 인자는 그 값을 구하지 않는 대신 썽크(thunk)라는 물체로 만든다.
;;; 썽크에는 프로시저 적용이 이루어진 환경과 그 인자 식이 들어 있다.


;;; 언어 실행기 고치기


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
	 (display "application!") (newline)
	 ;; (my-apply (my-eval (operator exp) env)
	 ;; 	(list-of-values (operands exp) env)))
	 (my-apply (actual-value (operator exp) env)
		   (operands exp)
		   env))
	(else
	 (error "Unknown expression type -- EVAL" exp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apply

(define (my-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
	 (display "<<my-apply : primitive-procedure>>")
	 (newline)
	 ;;(apply-primitive-procedure procedure arguments))
	 (apply-primitive-procedure 
	  procedure 
	  (list-of-arg-values arguments env)))     ; 바뀜

	((compound-procedure? procedure)
	 (display "<<my-apply : compound-procedure>>")
	 (newline)
	 (eval-sequence
	  (procedure-body procedure)
	  ;; (extend-environment
	  ;;  (procedure-parameters procedure)
	  ;;  arguments
	  (extend-environment
	   (procedure-parameters procedure)
	   (list-of-delayed-args arguments env) ; 바뀜

	   (procedure-environment procedure))))
	(else
	 (error
	  "Unknown procedure type -- APPLY" procedure))))


(define (actual-value exp env)
  ;; (display "actual-value :")
  ;; (display exp)
  ;; (display " ::: ")
  ;; (display env)
  ;; (newline)
  (force-it (my-eval exp env)))

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


;;; 썽크 표현

(define (force-it obj)
  (display "force-it")
  (display ":")
  (display obj)
  (newline)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))


(define (delay-it exp env)
  (list 'thunk exp env))


(define (thunk? obj)
  (tagged-list? obj 'thunk))


(define (thunk-exp thunk) (cadr thunk))


(define (thunk-env thunk) (caddr thunk))


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


(define the-global-environment (setup-environment))
;; (driver-loop)

(define (try a b)
  (if (= a 0) 1 b))

(try 0 (/ 1 0))

;;;--------------------------< ex 4.27 >--------------------------

;;;--------------------------< ex 4.28 >--------------------------

;;;--------------------------< ex 4.29 >--------------------------

;;;--------------------------< ex 4.30 >--------------------------

;;;--------------------------< ex 4.31 >--------------------------


;;;=================================================
;;; 4.2.3 제때셈 리스트와 스트림


;
;
;




;;;--------------------------< ex 4.32 >--------------------------



;;;--------------------------< ex 4.33 >--------------------------



;;;--------------------------< ex 4.34 >--------------------------


