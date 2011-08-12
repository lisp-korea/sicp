
;; racket -l r5rs/run


(load "../4.1/basic-eval-likerivers12.scm")
(load "../4.1/basic-eval-anal-likerivers12.scm")

;;===================================================================

(define (my-eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
	 (analyze-self-evaluating exp))
	((quoted? exp) (analyze-quoted exp))
	((variable? exp) (analyze-variable exp))
	((assignment? exp) (analyze-assignment exp))
	((definition? exp) (analyze-definition exp))
	((if? exp) (analyze-if exp))
	((lambda? exp) (analyze-lambda exp))
	((begin? exp) (analyze-sequence (begin-actions exp)))
	((cond? exp) (analyze (cond->if exp)))

        ((let? exp) (analyze (let->combination exp))) ;**

	;; analyze에 추가
	((amb? exp) (analyze-amb exp))

	((application? exp) (analyze-application exp))
	(else
	 (error "Unknown expression type -- ANALYZE" exp))))


;; let 문법 추가 - from longfin's code
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))
(define (let-val binding) (cadr binding))

(define (make-combination operator operands) (cons operator operands))

(define (let->combination exp)
  ;;make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                                   (let-body exp))
                      (map let-val bindings))))

;;===================================================================

;;;;;;;;;;;;;;;;;
;;; 실행기의 구조
;;; p560

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))


(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;;;;;;;;;;
;;; 간단한 식
;;; p562

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
	     fail)))


(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
	       fail))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; 조건식과 차례식
;;; p563

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
	(cproc (analyze (if-consequent exp)))
	(aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
	     ;; 성공했을 때 할 일
	     (lambda (pred-value fail2)
	       (if (true? pred-value)
		   (cproc env succeed fail2)
		   (aproc env succeed fail2)))
	     ;; 실패했을 때 할 일
	     fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
	 ;; 성공했을 때 호출할 함수
	 (lambda (a-value fail2)
	   (b env succeed fail2))
	 ;; 실패했을 때 할 일
	 fail)))
  
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
	(error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; 정의와 덮어쓰기
;;; p564

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
	(vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (define-variable! var val env)
	       (succeed 'ok fail2))
	     fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)        ; *1*
	       (let ((old-value
		      (lookup-variable-value var env)))
		 (set-variable-value! var val env)
		 (succeed 'ok
			  (lambda ()    ; *2*
			    (set-variable-value! var
						 old-value
						 env)
			    (fail2)))))
	     fail))))


;;;;;;;;;;;;;;;;;;;;;;
;;; 프로시저 적용
;;; p566
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env 
	     (lambda (proc fail2)
	       (get-args aprocs
			 env
			 (lambda (args fail3)
			   (execute-application
			    proc args succeed fail3))
			 fail2))
	     fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
       ;; 성공했을 때 aprocs를 위해 할 일
       (lambda (arg fail2)
	 (get-args (cdr aprocs)
		   env
		   ;; 성공했을 때 get-args를 위해 할 일
		   (lambda (args fail3)
		     (succeed (cons arg args)
			      fail3))
		   fail2))
       fail)))


(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
	 (succeed (apply-primitive-procedure proc args)
		  fail))
	((compound-procedure? proc)
	 ((procedure-body proc)
	  (extend-environment (procedure-parameters proc)
			      args
			      (procedure-environment proc))
	  succeed
	  fail))
	(else
	 (error
	  "Unknown procedure type -- EXECUTE-APPLICATION"
	  proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices)
	    (fail)
	    ((car choices) env
	     succeed
	     (lambda ()
	       (try-next (cdr choices))))))
      (try-next cprocs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 드라이버 루프
;;; p569

(define input-prompt ";;; Amb-Eval input:")

(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
	  (try-again)
	  (begin
	    (newline)
	    (display ";;; Starting a new problem ")
	    (ambeval input
		     the-global-environment
		     ;; ambeval의 성공한 다음 할 일
		     (lambda (val next-alternative)
		       (announce-output output-prompt)
		       (user-print val)
		       (internal-loop next-alternative))
		     ;; ambeval의 실패한 다음 할 일
		     (lambda ()
		       (announce-output
			";;; There are no more values of")
		       (user-print input)
		       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";; There is no current problem")
     (driver-loop))))






;;---------------------------------------------------------------
;; from longfin's code
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '> >)
	(list '< <)
        (list '>= >=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
	(list 'display display)
	(list 'newline newline)
	(list 'reverse reverse)
	(list 'even? even?)
;;      more primitives
        ))

(define the-global-environment (setup-environment))

;; add error procedure

(define (error reason . args)
      (display "Error: ")
      (display reason)
      (for-each (lambda (arg) 
                  (display " ")
		  (write arg))
		args)
      (newline)
      (scheme-report-environment 5))
;;---------------------------------------------------------------

'amb-eval-loaded




;;;--------------------------------
;;; test code
;;; amb 실행기에서 수행

;; (define (require p)
;;   (if (not p) (amb)))

;; (define (an-element-of items)
;;   (require (not (null? items)))
;;   (amb (car items) (an-element-of (cdr items))))

;; (define (an-integer-starting-from n)
;;   (amb n (an-integer-starting-from (+ n 1))))

;; ;;; 피타고라스 수
;; ;;; i^2 + j^2 = k^2
;; ;; (define (a-pythagorean-triple-between low high)
;; ;;   ((lambda (i)
;; ;;      ((lambda (j)
;; ;; 	((lambda (k)
;; ;; 	   (require (= (+ (* i i) (* j j)) (* k k)))
;; ;; 	   (list i j k))
;; ;; 	 (an-integer-between j high)))
;; ;;       (an-integer-between i high)))
;; ;;    (an-integer-between low high)))

;; (define (a-pythagorean-triple-between low high)
;;   (let ((i (an-integer-between low high)))
;;     (let ((j (an-integer-between i high)))
;;       (let ((k (an-integer-between j high)))
;; 	(require (= (+ (* i i) (* j j)) (* k k)))
;; 	(list i j k)))))


;; (define (an-integer-between low high)
;;   (require (not (> low high)))
;;   (amb low (an-integer-between (+ 1 low) high)))

;; (a-pythagorean-triple-between 1 10)


;;;
;;;-------------------------------------------------------
