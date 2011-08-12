
;; racket -l r5rs/run


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 4 언어를 처리하는 기법
;;; Ch 4.3. Scheme 바꿔보기 - 비결정적 계산
;;; p538


;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;;; 매크로를 이용한 amb 구현

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



;;;==========================================
;;; 4.3.1. amb와 찾기
;;; p541

(load "./amb-eval-likerivers12.scm")
(driver-loop)

(define (require p)
  (if (not p) (amb)))


;;------------------
;; amb 실행기에서 실행
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(an-element-of '(1 2 3))
try-again
try-again



(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(an-integer-starting-from 10)
try-again
try-again

;; 체계적으로 갈림길을 찾는다.
;; 깊이 먼저 찾기 방식으로

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
try-again
try-again
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 논리 퍼즐
(load "./amb-eval-likerivers12.scm")
(driver-loop)

(define (require p)
  (if (not p) (amb)))


;;------------------
;; amb 실행기에서 실행
(define (distinct? list)
  (define (find el lst)
	 (cond
	  ((null? lst) false)
	  ((= el (car lst)) true)
	  (else (find el (cdr lst)))))		  
  (cond ((null? list) true)
		((= 1 (length list)) true)
		(else
		 (if (find (car list) (cdr list))
			 false
			 (distinct? (cdr list))))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)
;;->((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
try-again

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

(load "./amb-eval-likerivers12.scm")
(driver-loop)

(define (require p)
  (if (not p) (amb)))


;;------------------
;; amb 실행기에서 실행

;;Parsing natural language

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

;;(sentence (noun-phrase (article the) (noun cat))
;;          (verb eats))

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

(parse '(the cat eats))
;;(sentence (noun-phrase (article the) (noun cat)) (verb eats))
try-again


(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
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

(parse '(the student with the cat sleeps in the class))
try-again


(parse '(the professor lectures to the student with the cat))
try-again


;;;--------------------------< ex 4.45 >--------------------------

;;;--------------------------< ex 4.46 >--------------------------

;;;--------------------------< ex 4.47 >--------------------------

;;;--------------------------< ex 4.48 >--------------------------

;;;--------------------------< ex 4.49 >--------------------------




;;;==========================================
;;; 4.3.3 amb 실행기 구현
;;; p547

;;; 코드는 아래 파일에
;;; (load "./amb-eval-likerivers12.scm")


;;;--------------------------< ex 4.50 >--------------------------


(load "./amb-eval-likerivers12.scm")

;;--------
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))


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
        ((amb? exp) (analyze-amb exp))                ;**
		((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))


(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
			(let ((i (random (length choices))))
			  (let ((choosen (list-ref choices i)))
				(let ((rest (filter (lambda (c) (not (eq? c choosen))) choices)))
				  (choosen env
						   succeed
						   (lambda ()
							 (try-next rest))))))))
	  (try-next cprocs))))

;;-----
(driver-loop)

(define (require p)
  (if (not p) (amb)))

;;-
(define (distinct? list)
  (define (find el lst)
	 (cond
	  ((null? lst) false)
	  ((= el (car lst)) true)
	  (else (find el (cdr lst)))))		  
  (cond ((null? list) true)
		((= 1 (length list)) true)
		(else
		 (if (find (car list) (cdr list))
			 false
			 (distinct? (cdr list))))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)
;;->((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
try-again




;;;--------------------------< ex 4.51 >--------------------------


(load "./amb-eval-likerivers12.scm")

;;--------

(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))
(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
		((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
		((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
                 (set-variable-value! var val env)
                 (succeed 'ok fail2))
             fail))))

;;----

(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define count 0)

(let ((x (an-element-of '(a b c)))
	  (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

count

(define count 0)

(let ((x (an-element-of '(a b c)))
	  (y (an-element-of '(a b c))))
  (set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

count

;;;--------------------------< ex 4.52 >--------------------------

(load "./amb-eval-likerivers12.scm")

;;--------

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
		((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
		((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
		((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (if-fail-proc exp) (cadr exp))
(define (if-fail-onfailure exp) (caddr exp))

  
(define (analyze-if-fail exp)
  (let ((proc (analyze (if-fail-proc exp)))
        (fproc (analyze (if-fail-onfailure exp))))
    (lambda (env succeed fail)
	  (proc env
			;; go ahead.
			succeed
			;; override failure contiuation.
			(lambda ()
			  (fproc env succeed fail))))))

;;---
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(if-fail (let ((x (an-element-of '(1 3 5))))
		   (require (even? x))
		   x)
		 'all-odd)

(if-fail (let ((x (an-element-of '(1 3 5 8))))
		   (require (even? x))
		   x)
		 'all-odd)

;;;--------------------------< ex 4.53 >--------------------------

;;; 4.52부터 수행

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
		(b (an-element-of list2)))
	(require (prime? (+ a b)))
	(list a b)))

(define (prime? x)
  (define (iter n)
	(cond ((= n x) true)
		  ((= (remainder x n) 0) false)
		  (else
		   (iter (+ 1 n)))))
  (if (< x 2)
	  false
	  (iter 2)))

(let ((pairs '()))
 (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
			(permanent-set! pairs (cons p pairs))
			(amb))
		  pairs))


;;;--------------------------< ex 4.54 >--------------------------


(load "./amb-eval-likerivers12.scm")

;;--------


(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))



(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
		((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
		((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
		((ramb? exp) (analyze-ramb exp))
		((require? exp) (analyze-require exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
	(lambda (env succeed fail)
	  (pproc env
			 (lambda (pred-value fail2)
			   (if (true? (not pred-value))
				   (fail)
				   (succeed 'ok fail2)))
			 fail))))


;;-----
(driver-loop)

;; require 정의없이

;; test
(define (distinct? list)
  (define (find el lst)
	 (cond
	  ((null? lst) false)
	  ((= el (car lst)) true)
	  (else (find el (cdr lst)))))		  
  (cond ((null? list) true)
		((= 1 (length list)) true)
		(else
		 (if (find (car list) (cdr list))
			 false
			 (distinct? (cdr list))))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)
;;->((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
try-again
