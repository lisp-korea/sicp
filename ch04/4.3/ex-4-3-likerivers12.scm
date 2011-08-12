
;; racket -l r5rs/run


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 4 언어를 처리하는 기법


;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


(define-syntax amb
  (syntax-rules ()
    ((amb) (try-again))
    ((amb x) x)
    ((amb x . xs)
     (amb+ (lambda () x)
           (lambda () (amb . xs))))))

(define (try-again)
  (if (null? amb-stack)
      (error "amb search tree exhausted")
      (let ((r (car amb-stack)))
        (set! amb-stack (cdr amb-stack))
        (r))))

(define (amb-reset)
  (set! amb-stack '()))
      
(define amb-stack '())

(define (amb+ a b)
  (define s '())
  (set! s amb-stack)
  (call/cc
   (lambda (r)
     (call/cc
      (lambda (c)
        (set! amb-stack 
              (cons c amb-stack))
        (r (a))))
	 (set! amb-stack s)
     (b))))  

(define call/cc call-with-current-continuation)

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


;;; Ch 4.3. Scheme 바꿔보기 - 비결정적 계산
;;; p538



;;;==========================================
;;; 4.3.1. amb와 찾기
;;; p541

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))


;; 체계적으로 갈림기을 찾는다.
;; 깊이 먼저 찾기 방식으로

;;;;;;;;;;;;;;;;;;;;;;;;
;;; 드라이버 루프
;;; p544



;;;--------------------------< ex 4.35 >--------------------------
;;; 피타고라스 수
;;; i^2 + j^2 = k^2
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
	(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))


(define (an-integer-between low high)
  (require (not (> low high)))
  (amb low (an-integer-between (+ 1 low) high)))

(a-pythagorean-triple-between 1 10)

;;;--------------------------< ex 4.36 >--------------------------


;;;--------------------------< ex 4.37 >--------------------------
;;; 더 효율적인가?

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
	(hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
	(require (>= hsq ksq))
	(let ((k (sqrt ksq)))
	  (require (integer? k))
	  (list i j k))))))





;;;==========================================
;;; 4.3.2 비결정적 프로그램 짜기
;;; p547

;;; 논리 퍼즐

;; (define (multiple-dwelling)
;; ...
;; )



;;;--------------------------< ex 4.38 >--------------------------


;;;--------------------------< ex 4.39 >--------------------------

;;;--------------------------< ex 4.40 >--------------------------

;;;--------------------------< ex 4.41 >--------------------------

;;;--------------------------< ex 4.42 >--------------------------
;;; 거짓말쟁이들 퍼즐

;;;--------------------------< ex 4.43 >--------------------------

;;;--------------------------< ex 4.44 >--------------------------
;;; 8퀸


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 자연어 문법 분석
;;; p551


;; 낱말의 품사
(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))


;; 각 문법 요소가 어떻게 더 단순한 요소로 짜맞추어지는지 밝히는 규칙
;; (setence (noun-phrase (article the) (noun cat))
;; 	 (verb eats))

;.
;.
;.

(define (parse-sentence)
  (list 'sentence
	(parse-noun-phrase)
	(parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
	(parse-word articles)
	(parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))


(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

;;
(parse '(the cat eats))



(define prepositions '(prep for to in by with))


(define (parse-prepositional-phrase)
  (list 'prep-phrase
	(parse-word prepositions)
	(parse-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
	 (maybe-extend (list 'verb-phrase
			     verb-phrase
			     (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))


(define (parse-simple-noun-phrae)
  (list 'simple-noun-phrase
	(parse-word articles)
	(parse-word nouns)))


(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
	 (maybe-extend (list 'noun-phrase
			     noun-phrase
			     (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

;;
(parse '(the student with the cat sleeps in the class))


;;;--------------------------< ex 4.45 >--------------------------

;;;--------------------------< ex 4.46 >--------------------------

;;;--------------------------< ex 4.47 >--------------------------

;;;--------------------------< ex 4.48 >--------------------------

;;;--------------------------< ex 4.49 >--------------------------




;;;==========================================
;;; 4.3.3 amb 실행기 구현
;;; p547


;;;;;;;;;;;;;;;;;
;;; 실행기 프로시저와 앞으로 할 일



;;;;;;;;;;;;;;;;;
;;; 실행기의 구조
;;; p560

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))


;; analyze에 추가
;;((amb? exp) (analyze-amb exp))


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
	(aproc (map analyze (operands exp))))
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


;;;--------------------------< ex 4.50 >--------------------------

;;;--------------------------< ex 4.51 >--------------------------

;;;--------------------------< ex 4.52 >--------------------------

;;;--------------------------< ex 4.53 >--------------------------

;;;--------------------------< ex 4.54 >--------------------------
