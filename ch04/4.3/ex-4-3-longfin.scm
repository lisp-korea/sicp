;; 4.3 Variations on a Scheme -- Nondeterministic Computing

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
		(b (an-element-of list2)))
	(require (prime? (+ a b)))
	(list a b)))

(define (prime? x)
  (define (iter n)
	(cond ((= n x) #t)
		  ((= (remainder x n) 0) #f)
		  (else
		   (iter (+ 1 n)))))
  (if (< x 2)
	  #f
	  (iter 2)))

;; 4.3.1 Amb and Search


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


(list (amb 1 2 3) (amb 'a 'b))

(try-again)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))


;; Driver loop

;; ;;; Amb-Eval input:
;; (prime-sum-pair '(1 3 5 8) '(20 35 110))
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; (3 20)
;; ;;; Amb-Eval input:
;; try-again
;; ;;; Amb-Eval value:
;; (3 110)
;; ;;; Amb-Eval input:
;; try-again
;; ;;; Amb-Eval value:
;; (8 35)
;; ;;; Amb-Eval input:
;; try-again
;; ;;; There are no more values of
;; (prime-sum-pair (quote (1 3 5 8)) (quote (20 35 110)))
;; ;;; Amb-Eval input:
;; (prime-sum-pair '(19 27 30) '(11 36 58))
;; ;;; Starting a new problem ;;; Amb-Eval value: (30 11)

;; ex 4.35

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
	(let ((j (an-integer-between i high)))
	  (let ((k (an-integer-between j high)))
		(require (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))

(define (an-integer-between low high)
  (require (not (> low high)))
  (amb low (an-integer-between (+ 1 low) high)))

;; ex 4.36

;; replacing an-integer-between by an-integer-starting-from...

(define (a-pythagorean-triple)
  (let ((i (an-integer-starting-from 1)))
	(let ((j (an-integer-starting-from i)))
	  (let ((k (an-integer-starting-from j)))
		(require (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))

;; it goes to infinite loop because (try-again) produces only k.(continuation of k is top of amb-stack)

;; 1^2 + 1^2 vs 1^2
;; 1^2 + 1^2 vs 2^2
;; 1^2 + 1^2 vs 3^2
;; 1^2 + 1^2 vs 4^2
;; 1^2 + 1^2 vs 5^2
;; ...

(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
	(let ((i (an-integer-between 1 k)))
	  (let ((j (an-integer-between 1 i)))
		(require (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))

;; 1^2 + 1^2 vs 1^2
;; 1^2 + 1^2 vs 2^2
;; 2^2 + 1^2 vs 2^2
;; 2^2 + 2^2 vs 2^2
;; 1^2 + 1^2 vs 3^2
;; ...


;; ex 4.37
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
		(hsq (* high high)))
	(let ((j (an-integer-between i high)))
	  (let ((ksq (+ (* i i) (* j j))))
		(require (>= hsq ksq))
		(let ((k (sqrt ksq)))
		  (require (integer? k))
		  (list i j k))))))

;; hsq = high * high
;; ksq = i^2 + j^2
;; filter hsq >= ksq
;; filter sqrt ksq(=k) is integer

;; (a-pythagorean-triple-between 1 10)

;; high = 10
;; low = 1

;; [amb-1]
;; i = 1

;; hsq = 100

;; [amb-2]
;; j = 1
;; ksq = 2

;; require 100 >= 2
;; k = sqrt 2

;; require k is integer => false : to amb-2

;; [amb-2]
;; j == i :to amb-1

;; [amb-1]
;; i = 2

;; hsq = 100

;; [amb-2]
;; j = 1
;; ksq = 5

;; require 100 >= 5
;; k = sqrt 5

;; require k isn't integer => false : to amb-2

;; ...


;; it uses only 2 continuation.(i, j) so time complexity is O(n^2)[4.35's solution is O(n^3)]

;; 4.3.2 Examples of Nondeterministic Programs

;; Logic Puzzles

(define (distinct? list)
  (define (find el lst)
	 (cond
	  ((null? lst) #f)
	  ((= el (car lst)) #t)
	  (else (find el (cdr lst)))))		  
  (cond ((null? list) #t)
		((= 1 (length list)) #t)
		(else
		 (if (find (car list) (cdr list))
			 #f
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


;; ex 4.38

		  
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
;;	(require (not (= (abs (- smith fletcher)) 1)))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(list (list 'baker baker)
		  (list 'cooper cooper)
		  (list 'fletcher fletcher)
		  (list 'miller miller)
		  (list 'smith smith))))

;; > (multiple-dwelling)
;; ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
;; > (try-again)
;; ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
;; > (try-again)
;; ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
;; > (try-again)
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; > (try-again)
;; ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
;; > (try-again)
;; ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
;; > (try-again)
;; ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))


;; ex 4.39

;; Ordering condition doesn't effect whole time complexity.(it's effected only # of branches), but if all predicate on condition aren't same, ordering condition can effect performance...(if (distinct?) costs more than '=, checking '= first is efficient way.)


;; ex 4.40

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5)))
	(require (not (= baker 5)))
	(let (cooper (amb 1 2 3 4 5))
	  (require (not (= cooper 1)))
	  (let (fletcher (amb 1 2 3 4 5))
		(require (not (= fletcher 5)))
		(require (not (= fletcher 1)))
		(let ((miller (amb 1 2 3 4 5))
			  (smith (amb 1 2 3 4 5)))
		  (require
		   (distinct? (list baker cooper fletcher miller smith)))
		  (require (> miller cooper))
		  (require (not (= (abs (- smith fletcher)) 1)))
		  (require (not (= (abs (- fletcher cooper)) 1)))
		  (list (list 'baker baker)
				(list 'cooper cooper)
				(list 'fletcher fletcher)
				(list 'miller miller)
				(list 'smith smith)))))))

;; ex 4.41

(define (multiple-dwelling2)
  (let ((*answer* '()))
	(let ((baker '(1 2 3 4 5)))
	  (for-each (lambda (b)
				  (let ((copper '(1 2 3 4 5)))
					(for-each (lambda (c)
								(let ((fletcher '(1 2 3 4 5)))
								  (for-each (lambda (f)
											  (let ((miller '(1 2 3 4 5)))
												(for-each (lambda (m)
															(let ((smith '(1 2 3 4 5)))
															  (for-each (lambda (s)
																		  (let ((result (list b c f m s)))
																			(if (distinct? result)
																				(if (not (= b 5))
																					(if (not (= c 1))
																						(if (and (not (= f 5))
																								 (not (= f 1)))
																							(if (> m c)
																								(if (not (= (abs (- s f)) 1))
																									(if (not (= (abs (- f c)) 1))
																										(set! *answer* result))))))))))
																		smith)))
														  miller)))
											fletcher)))
							  copper))
				  baker)))
	*answer*))
  


;; ex 4.42

(define (solve-lairs)
  (let ((betty (amb 1 2 3 4 5))
		(ethel (amb 1 2 3 4 5))
		(kitty (amb 1 2 3 4 5))
		(joan (amb 1 2 3 4 5))
		(mary (amb 1 2 3 4 5)))
	(require (distinct? (list betty
							  ethel
							  kitty
							  joan
							  mary)))
	(require (or (and (= kitty 2) (not (= betty 3)))
				 (and (not (= kitty 2)) (= betty 3))))
	(require (or (and (= ethel 1) (not (= joan 2)))
				 (and (not (= ethel 1)) (= joan 2))))
	(require (or (and (= joan 3) (not (= ethel 5)))
				 (and (not (= joan 3)) (= ethel 5))))
	(require (or (and (= kitty 2) (not (= mary 4)))
				 (and (not (= kitty 2)) (= mary 4))))
	(require (or (and (= mary 4) (not (= betty 1)))
				 (and (not (= mary 4)) (= betty 1))))

	(list (list 'betty betty)
		  (list 'ethel ethel)
		  (list 'kitty kitty)
		  (list 'joan joan)
		  (list 'mary mary))))

;; ex 4.43
(define (distinct? list)
  (define (find el lst)
	(let ((pred (if (symbol? el) eq? =)))
	  (cond
	   ((null? lst) #f)
	   ((pred el (car lst)) #t)
	   (else (find el (cdr lst))))))
  (cond ((null? list) #t)
		((= 1 (length list)) #t)
		(else
		 (if (find (car list) (cdr list))
			 #f
			 (distinct? (cdr list))))))

(define (who-is-lornas-father)
  (let ((mary (amb 'moore 'downing 'hall 'hood 'parker)))
;;	(require (eq? mary 'moore))
	(let ((gabrielle (amb 'moore 'downing 'hall 'hood 'parker))
		  (lorna (amb 'moore 'downing 'hall 'hood 'parker))
		  (rosalind (amb 'moore 'downing 'hall 'hood 'parker))
		  (melissa (amb 'moore 'downing 'hall 'hood 'parker)))
	  (require (distinct? (list mary gabrielle lorna rosalind melissa)))
	  (require (not (eq? gabrielle 'hood))) ;; hood
	  (require (not (eq? lorna 'moore))) ;; moore
	  (require (not (eq? rosalind 'hall))) ;; hall
	  (require (eq? melissa 'hood)) ;; downing
	  lorna)))

;; ex 4.44

(define (solve-eight-queens)
  (let ((q1 (amb 1 2 3 4 5 6 7 8))
		(q2 (amb 1 2 3 4 5 6 7 8))
		(q3 (amb 1 2 3 4 5 6 7 8))
		(q4 (amb 1 2 3 4 5 6 7 8))
		(q5 (amb 1 2 3 4 5 6 7 8))
		(q6 (amb 1 2 3 4 5 6 7 8))
		(q7 (amb 1 2 3 4 5 6 7 8))
		(q8 (amb 1 2 3 4 5 6 7 8)))
	(require (distinct? (list q1 q2 q3 q4 q5 q6 q7 q8)))
	(require (distinct? (list (- q1 1)
	 						  (- q2 2)
	 						  (- q3 3)
							  (- q4 4)
							  (- q5 5)
							  (- q6 6)
							  (- q7 7)
							  (- q8 8))))
	(require (distinct? (list (+ q1 1)
							  (+ q2 2)
							  (+ q3 3)
							  (+ q4 4)
							  (+ q5 5)
							  (+ q6 6)
							  (+ q7 7)
							  (+ q8 8))))
	(list q1 q2 q3 q4 q5 q6 q7 q8)))
	
			
;; Parsing natural language


;; setup amb evaluator
(load "ch4-ambeval.scm")
(define the-global-environment (setup-environment))

(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

;; (sentence (noun-phrase (article the) (noun cat))
;;           (verb eats))

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

(parse '(the professor lectures to the student with the cat))

;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase (verb-phrase (verb lectures)
;; 						   (prep-phrase (prep to)
;; 										(simple-noun-phrase (article the) (noun student))))
;; 			  (prep-phrase (prep with)
;; 						   (simple-noun-phrase (article the) (noun cat)))))

;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase (prep to)
;;                (noun-phrase
;;                 (simple-noun-phrase
;;                  (article the) (noun student))
;;                 (prep-phrase (prep with)
;;                              (simple-noun-phrase
;;                               (article the) (noun cat)))))))


;; ex 4.45


;; sentence := noun phrase + verb phrase

;; noun phrase := article word + noun word
;; noun phrase := noun phrase + prepositional phrase

;; verb phrase := verb word
;; verb phrase := verb word + prepositional phrase

;; prepositional phrase := prepositional word + noun phrase

;; "the professor lectures to the student in the class with the cat"
;; 1. (the professor)
;; 2. (lectures)
;; 3. (to)
;; 4. (the student)
;; 5. (in)
;; 6. (the class)
;; 7. (with)
;; 8. (the cat)

(parse '(the professor lectures to the student in the class with the cat))

;; (sentence
;;  (simple-noun-phrase (article the) (noun professor)) ;; 1
;;  (verb-phrase
;;   (verb-phrase
;;    (verb-phrase
;; 	(verb lectures) ;;2
;; 	(prep-phrase
;; 	 (prep to) ;; 3
;; 	 (simple-noun-phrase (article the) (noun student)))) ;; 4	
;;    (prep-phrase
;; 	(prep in) ;; 5
;; 	(simple-noun-phrase (article the) (noun class)))) ;; 6
;;   (prep-phrase
;;    (prep with) ;; 7
;;    (simple-noun-phrase (article the) (noun cat))))) ;; 8

;; [1 (((2 <3 4>) <5 6>) <7 8>)]

try-again

;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase
;; 	(prep to)
;; 	(simple-noun-phrase (article the) (noun student))))
;;   (prep-phrase
;;    (prep in)
;;    (noun-phrase
;; 	(simple-noun-phrase (article the) (noun class))
;; 	(prep-phrase
;; 	 (prep with)
;; 	 (simple-noun-phrase (article the) (noun cat)))))))

;; [1 ((2 <3 4>) (<5 (6 <7 8>)))]

;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase
;; 	(prep to)
;; 	(noun-phrase
;; 	 (simple-noun-phrase (article the) (noun student))
;; 	 (prep-phrase
;; 	  (prep in)
;; 	  (simple-noun-phrase (article the) (noun class))))))
;;   (prep-phrase
;;    (prep with)
;;    (simple-noun-phrase (article the) (noun cat)))))

;; [1 ((2 <3 (4 <5 6>)>) <7 8>)]

;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase
;;    (prep to)
;;    (noun-phrase
;; 	(noun-phrase
;; 	 (simple-noun-phrase (article the) (noun student))
;; 	 (prep-phrase
;; 	  (prep in)
;; 	  (simple-noun-phrase (article the) (noun class))))
;; 	(prep-phrase
;; 	 (prep with)
;; 	 (simple-noun-phrase (article the) (noun cat)))))))

;; [1 (2 <3 ((4 <5 6>) <7 8>)>)]

;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase
;;    (prep to)
;;    (noun-phrase
;; 	(simple-noun-phrase (article the) (noun student))
;; 	(prep-phrase
;; 	 (prep in)
;; 	 (noun-phrase
;; 	  (simple-noun-phrase (article the) (noun class))
;; 	  (prep-phrase
;; 	   (prep with)
;; 	   (simple-noun-phrase (article the) (noun cat)))))))))

;; [1 (2 <3 (4 <5 (6 <7 8>)>))]


;; ex 4.46

;; if amb evaluator evaluates right to left...

;; (parse '(the cat eats))

;; (set! *unparsed* '(the cat eats))
;; (parse-sentence)

;; (parse-word verbs)
;; found-word = the
;; it's not found in verbs!


;; ex 4.47

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

;; it parses this illegal sentence and goes infinte loop. because it matches 2nd direction and it also calls parse-verb-phrase.

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

;; ex 4.48

(define adjectives '(adj big small old))
(define adverbs '(adv slowly quickly))

(define (parse-noun-phrase)
  (define (inner rules)
	(require (not (null? rules)))
	(amb
	 (parse-word nouns)
	 ((car rules) rules)
	 (inner (cdr rules))))
  (let ((article-noun-rule (lambda (rules)
							 (list 'noun-phrase
								   (parse-word articles)
								   (inner rules))))
		(adjectives-noun-rule (lambda (rules)
								(list 'noun-phrase
									  (parse-word adjectives)
									  (inner rules))))
		(noun-prep-rule (lambda (rules)
						  (list 'noun-phrase
								(parse-word nouns)
								(parse-prepositional-phrase)))))
	(inner (list article-noun-rule
				 adjectives-noun-rule
				 noun-prep-rule))))

(define (parse-verb-phrase)
  (define (inner rules)
	(require (not (null? rules)))
	(amb
	 (parse-word verbs)
	 ((car rules) rules)
	 (inner (cdr rules))))
  (let ((adverb-verb-rule (lambda (rules)
							(list 'verb-phrase
								  (parse-word adverbs)
								  (inner rules))))
		(verb-prep-rule (lambda (rules)
						  (list 'verb-phrase
								(parse-word verbs)
								(parse-prepositional-phrase)))))
	(inner (list
			adverb-verb-rule
			verb-prep-rule))))


;; ex 4.49


(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (set! *unparsed* (cdr *unparsed*))
  (define (generate lst)
	(require (not (null? lst)))
	(amb
	 (car lst)
	 (generate (cdr lst))))
  (list (car word-list) (generate (cdr word-list))))


;; 4.3.3 Implementing the amb Evaluator

;; Execution procedures and continuations

;; Failure continuations are constructed by
;; - amb expressions -- to provide a mechanism to make alternative choices if the current choice made by the amb expression leads to a dead end;
;; - the top-level driver -- to provide a mechanism to report failure when the choices are exhausted;
;; - assignments -- to intercept failures and undo assignments during backtracking.

;; Failures are initiated only when a dead end is encountered. This occurs
;; - if the user program executes (amb);
;; - if the user types try-again at the top-level driver.

;; Failure continuations are also called during processing of a failure:
;; - When the failure continuation created by an assignment finishes undoing a side effect, it calls the failure continuation it intercepted, in order to propagate the failure back to the choice point that led to this assignment or to the top level.
;; - When the failure continuation for an amb runs out of choices, it calls the failure continuation that was originally given to the amb, in order to propagate the failure back to the previous choice point or to the top level.


;; analyze from 4.1.6, with clause from 4.3.3 added
;; and also support for Let
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
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;;; Support for Let (as noted in footnote 56, p.428)

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
                     
;; Structure of the evaluator

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))


;; (ambeval <exp> the-global-environment
;;          (lambda (value fail) value)
;;          (lambda () 'failed))



;; Simple expressions

;; before
;; (define (analyze-self-evaluating exp)
;;   (lambda (env) exp))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

;; before
;; (define (analyze-quoted exp)
;;   (let ((qval (text-of-quotation exp)))
;; 	(lambda (env) qval)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

;; before
;; (define (analyze-variable exp)
;;   (lambda (env) (lookup-variable-value exp env)))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

;; before
;; (define (analyze-lambda exp)
;;   (let ((vars (lambda-parameters exp))
;; 		(bproc (analyze-sequence (lambda-body exp))))
;; 	(lambda (env) (make-procedure-with-analyze vars bproc env))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
			   fail))))


;; Conditionals and Sequences
;; before
;; (define (analyze-if exp)
;;   (let ((pproc (analyze (if-predicate exp)))
;; 		(cporc (analyze (if-consequent exp)))
;; 		(aproc (analyze (if-alternative exp))))
;; 	(lambda (env)
;; 	  (if (true? (pproc env))
;; 		  (cporc env)
;; 		  (aproc env)))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
	(lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
			 fail))))

;; before
;; (define (analyze-sequence exps)
;;   (define (sequentially proc1 proc2)
;; 	(lambda (env) (proc1 env) (proc2 env)))
;;   (define (loop first-proc rest-procs)
;; 	(if (null? rest-procs)
;; 		first-proc
;; 		(loop (sequentially first-proc (car rest-procs))
;; 			  (cdr rest-procs))))
;;   (let ((procs (map analyze exps)))
;; 	(if (null? procs)
;; 		(error "Empty sequence -- ANALYZE"))
;; 	(loop (car procs) (cdr procs))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
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

;; Definitions and assignments

;; before
;; (define (analyze-definition exp)
;;   (let ((var (definition-variable exp))
;; 		   (vproc (analyze (definition-value exp))))
;; 	(lambda (env)
;; 	  (define-variable! var (vproc env) env)
;; 	  'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
	  (vproc env
			 (lambda (val fail2)
			   (define-variable! var val env)
			   (succeed 'ok fail2))
			 fail))))


;; before
;; (define (analyze-assignment exp)
;;   (let ((var (assignment-variable exp))
;; 		(vproc (analyze (assignment-value exp))))
;; 	(lambda (env)
;; 	  (set-variable-value! var (vproc env) env)
;; 	  'ok)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
	  (vproc env
			 (lambda (val fail2) ; *1*
			   (let ((old-value
					  (lookup-variable-value var env)))
				 (set-variable-value! var val env)
				 (succeed 'ok
						  (lambda () ; *2*
							(set-variable-value! var
												 old-value
												 env)
							(fail2)))))
			   fail))))

;; Procedure applications

;; before
;; (define (analyze-application exp)
;;   (let ((fproc (analyze (operator exp)))
;; 		(aprocs (map analyze (operands exp))))
;; 	(lambda (env)
;; 	  (execute-application (fproc env)
;; 						   (map (lambda (aproc) (aproc env))
;; 								aprocs)))))

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
	   ;; success continuation for this aproc
	   (lambda (arg fail2)
		 (get-args (cdr aprocs)
				   env
				   ;; success continuation for recursive
				   ;; call to get-args
				   (lambda (args fail3)
					 (succeed (cons arg args)
							  fail3))
				   fail2))
	   fail)))


;; before
;; (define (execute-application proc args)
;;   (cond ((primitive-procedure? proc)
;; 		 (apply-primitive-procedure proc args))
;; 		((compound-procedure? proc)
;; 		 ((procedure-body proc)
;; 		  (extend-environment (procedure-parameters proc)
;; 							  args
;; 							  (procedure-environment proc))))
;; 		(else
;; 		 (error
;; 		  "Unknown procedure type -- EXECUTE-APPLICATION"
;; 		  proc))))

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

;; Evaluating amb expressions

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


;; Driver loop

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
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


;; ex 4.50
;; using mit-scheme...

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


