;; 4.4 Logic Programming


;; 4.4.1 Deductive Information Retrieval

;; A sample data base

;; Ben's information
;; (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
;; (job (Bitdiddle Ben) (computer wizard))
;; (salary (Bitdiddle Ben) 60000)

;; Alyssa's information
;; (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
;; (job (Hacker Alyssa P) (computer programmer))
;; (salary (Hacker Alyssa P) 40000)
;; (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

;; Fect's information
;; (address (Fect Cy D) (Cambridge (Ames Street) 3))
;; (job (Fect Cy D) (computer programmer))
;; (salary (Fect Cy D) 35000)
;; (supervisor (Fect Cy D) (Bitdiddle Ben))

;; Tweakit's information
;; (address (Tweakit Lem E) (Boston (Bay State Road) 22))
;; (job (Tweakit Lem E) (computer technician))
;; (salary (Tweakit Lem E) 25000)
;; (supervisor (Tweakit Lem E) (Bitdiddle Ben))

;; Reasoner's information
;; (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
;; (job (Reasoner Louis) (computer programmer trainee))
;; (salary (Reasoner Louis) 30000)
;; (supervisor (Reasoner Louis) (Hacker Alyssa P))

;; Warbucks's information
;; (supervisor (Bitdiddle Ben) (Warbucks Oliver))
;; (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
;; (job (Warbucks Oliver) (administration big wheel))
;; (salary (Warbucks Oliver) 150000)

;; Scrooge's information
;; (address (Scrooge Eben) (Weston (Shady Lane) 10))
;; (job (Scrooge Eben) (accounting chief accountant))
;; (salary (Scrooge Eben) 75000)
;; (supervisor (Scrooge Eben) (Warbucks Oliver))

;; Cratchet's information
;; (address (Cratchet Robert) (Allston (N Harvard Street) 16))
;; (job (Cratchet Robert) (accounting scrivener))
;; (salary (Cratchet Robert) 18000)
;; (supervisor (Cratchet Robert) (Scrooge Eben))

;; Aull's information
;; (address (Aull DeWitt) (Slumerville (Onion Square) 5))
;; (job (Aull DeWitt) (administration secretary))
;; (salary (Aull DeWitt) 25000)
;; (supervisor (Aull DeWitt) (Warbucks Oliver))


;; (can-do-job (computer wizard) (computer programmer))
;; (can-do-job (computer wizard) (computer technician))

;; (can-do-job (computer programmer)
;;             (computer programmer trainee))

;; (can-do-job (administration secretary)
;;             (administration big wheel))

;; Simple queries

;; Query input:
(job ?x (computer programmer))

;; Query results:
(job (Hacker Alyssa P) (computer programmer))
(job (Fect Cy D) (computer programmer))

;; if ...
(address ?x ?y)

;; then list all the employees' addresses.

;; if ...
(supervisor ?x ?x)

;; doesn't match.(supervise themselves)


;; query
(job ?x (computer ?type))

;; result
(job (Bitdiddle Ben) (computer wizard))
(job (Hacker Alyssa P) (computer programmer))
(job (Fect Cy D) (computer programmer))
(job (Tweakit Lem E) (computer technician))

;; it doesn't match
(job (Reasoner Louis) (computer programmer trainee))

;; it matches(?type = (programmer trainee))
(job ?x (computer . ?type)) 


;; ex 4.55

;; a.
(supervise ?x (Ben Bitdiddle))

;; b.
(job ?x (accounting ?y))

;; c.
(address ?person (Slumerville . ?rest))


;; Compound queries

;; input
(and (job ?person (computer programmer))
     (address ?person ?where))

;; output
(and (job (Hacker Alyssa P) (computer programmer))
     (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(and (job (Fect Cy D) (computer programmer))
     (address (Fect Cy D) (Cambridge (Ames Street) 3)))

;; input
(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Alyssa P)))

;; output
(or (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
    (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))
(or (supervisor (Fect Cy D) (Bitdiddle Ben))
    (supervisor (Fect Cy D) (Hacker Alyssa P)))
(or (supervisor (Tweakit Lem E) (Bitdiddle Ben))
    (supervisor (Tweakit Lem E) (Hacker Alyssa P)))
(or (supervisor (Reasoner Louis) (Bitdiddle Ben))
    (supervisor (Reasoner Louis) (Hacker Alyssa P)))

;; find all people supervised by Ben Bitdiddle who are not computer programmers.
(and (supervisor ?x (Bitdiddle Ben))
     (not (job ?x (computer programmer))))

;; find all people whose salary is greater than $30,000
(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))


;; ex. 4.56

;; a.
(and (supervisor ?person (Bitdiddle Ben))
	 (address ?person ?addr))

;; b.
(and (salary (Bitdiddle Ben) ?salary-of-ben)
	 (salary ?person ?amount)
	 (lisp-value > ?amount ?salary-of-ben))

;; c.
(and (supervisor ?person ?boss)
	 (not (job ?person (computer . ?rest))))

;; Rules

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

;; input
(lives-near ?x (Bitdiddle Ben))

;; result
(lives-near (Reasoner Louis) (Bitdiddle Ben))
(lives-near (Aull DeWitt) (Bitdiddle Ben))


;; ex 4.57

(rule (alternative ?person1 ?person2)
	  (and (not (same ?person1 ?person2))
		   (or (and (job ?person1 ?job)
					(job ?person2 ?job))
			   (and (job ?person1 ?job1)
					(job ?person2 ?job2)
					(can-do-job ?job1 ?job2)))))

;; a.
(alternative (Fect Cy D) ?x)

;;b.
(and (salary ?person ?amount1)
	 (salary ?other ?amount2)
	 (alternative ?person ?other)
	 (lisp-value < ?amount1 ?amount2))

;; ex 4.58

(rule (big-shot ?person)
	  (and (job ?person (?depart . ?rest))
		   (not (and (supervisor ?person ?boss)
					 (job ?boss (?depart . ?dont-care))))))

;; ex 4.59

(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))

(meeting whole-company (Wednesday 4pm))

;; a.
(or (and (job (Ben Bitdiddle) (?depart . ?rest))
		 (meeting ?depart (Friday . ?time)))
	(meeting whole-company (Friday . ?time)))

;; b.
(rule (meeting-time ?person ?day-and-time)
	  (or (and (job ?person (?depart . ?rest))
			   (meeting ?depart ?day-and-time))
		  (meeting whole-company ?day-and-time)))
	  
;; c.
(and (meeting-time (Alyssa P Hacker) (Wednesday . ?time))
	 (meeting ?subject (Wednesday ?time)))

;; ex 4.60

(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))

;; two assertion are true.
;; add order condition...(assume dic< exists)

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))
		   (lisp-value dic< ?person-1 ?person-2)))

;; Logic as programs

(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
	  (append-to-form ?v ?y ?z))
				
;;; Query input:
(append-to-form (a b) (c d) ?z)
;;; Query results:
(append-to-form (a b) (c d) (a b c d))

;;; Query input:
(append-to-form (a b) ?y (a b c d))
;;; Query results:
(append-to-form (a b) (c d) (a b c d))

;;; Query input:
(append-to-form ?x ?y (a b c d))
;;; Query results:
(append-to-form () (a b c d) (a b c d))
(append-to-form (a) (b c d) (a b c d))
(append-to-form (a b) (c d) (a b c d))
(append-to-form (a b c) (d) (a b c d))
(append-to-form (a b c d) () (a b c d))


;; ex 4.61

(rule (?x next-to ?y in (?x ?y . ?u)))
(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

;; Query input:
(?x next-to ?y in (1 (2 3) 4))

;; Query result
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))

;; Query input:
(?x next-to 1 in (2 1 3 1))

;; Query result
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))


;; ex 4.62
(rule (last-pair (?x) ?x))
(rule (last-pair (?y . ?ys) ?x)
	  (last-pair ?ys ?x))
;; can't check (last-pair ?x (3)). (it seems so strange.)


;; ex 4.63

(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)
 
(rule (grandfather-son ?g ?s)
	  (and (son ?g ?f)
		   (son ?f ?s)))
(rule (father-son ?m ?s)
	  (or (son ?m ?s)
		  (and (wife ?m ?w)
			   (son ?w ?s))))


				 
	  
;; 4.4.2 How the Query System Works

;; Pattern matching

;; ((a b) c (a b)) matches (?x c ?x) [?x = (a b)] it also matches (?x c ?y)[?x = (a b), ?y = (a b)]. but doesn't match (?x a ?x).


;; Stream of frames

;; In our system, a query takes an input stream of frames and performs the above matching operation for every frame in the stream, as indicated in figure 4.4. 

;; Compound queries

;; (and (can-do-job ?x (computer programmer trainee))
;;     (job ?person ?x))
;; process (can-do-job ... ) first, and (job ...). (logical conjunction)

;; (or (can-do-job ?x (computer programmer trainee))
;; 	(can-do-job ?y (administration secretary)))
;; process two assertion parallelly. (process first with original frames and next, then merge result)(logical disjunction)

;; (not (job ?x (computer programmer)))
;; check all frames and remove it satisfies assertion. (logical negation)


;; Unification

;; unifying (?x a ?y) and (?y ?z a) will specify a frame in which ?x, ?y, and ?z must all be bound to a.

;; With complex patterns, performing unification may seem to require deduction.
;; To unify (?x ?x) and ((a ?y c) (a b ?z))
;; ?x  =  (a ?y c)
;; ?x  =  (a b ?z)
;; (a ?y c)  =  (a b ?z)
;; a  =  a, ?y  =  b, c  =  ?z,
;; ?x = (a b c)

;; (?x a) and ((b ?y) ?z)
;; ?x = (b ?y)
;; ?z = a
;; ?y = ????


;; Applying rules

;; (lives-near ?x (Hacker Alyssa P))
;; 1. check assertions. (fail)
;; 2. check rules. (success)
;; 3. bind pattern variables(?x, (Hacker Alyssa P))
;; 4. apply assertions in rule

;; Just as procedure definitions are the means of abstraction in Lisp, rule definitions are the means of abstraction in the query language.


;; Simple queries

;; Given the query pattern and a stream of frames, we produce, for each frame in the input stream, two streams:
;; 1. a stream of extended frames obtained by matching the pattern against all assertions in the data base (using the pattern matcher),
;; 2. a stream of extended frames obtained by applying all possible rules (using the unifier)
;; These streams (one for each frame in the input stream) are now all combined to form one large stream, which therefore consists of all the ways that any of the frames in the original input stream can be extended to produce a match with the given pattern.


;; The query evaluator and the driver loop

;; (qeval <query> <stream of frames>) => <stream of frames>

;; assert isn't query.(special form)
;; (assert! (job (Bitdiddle Ben) (computer wizard)))
;; (assert! (rule (wheel ?person)
;;                (and (supervisor ?middle-manager ?person)
;;                     (supervisor ?x ?middle-manager))))



;; 4.4.3 Is Logic Programming Mathematical Logic?


;; (and (job ?x (computer programmer))
;;      (supervisor ?x ?y))

;; or

;; (and (supervisor ?x ?y)
;;      (job ?x (computer programmer)))

;; two queries are logically equivalent.(same for *what*), but aren't same for *how*.


;; Infinite loop
;; (assert! (married Minnie Mickey))
;; (married Mickey ?who) => none[it doesn't match (married Minnie Mickey)]

;; (assert! (rule (married ?x ?y)
;;                (married ?y ?x)))
;; (married Mickey ?who)
;; (married ?x ?y) [?x = Mickey, ?y = ?who]
;; (married ?who Mickey)
;; (married ?x ?y) [?x = ?who, ?y = Mickey]
;; (married Mickey ?who)
;; ...
;; => fall in the infinite loop.


;; Problems with not

;; 1. (and (supervisor ?x ?y)
;;         (not (job ?x (computer programmer))))
;; 2.(and (not (job ?x (computer programmer)))
;;        (supervisor ?x ?y))

;; case 1.
;; 1. (supervisor ?x ?y) => fetch all supervisor information from database, and bind ?x and ?y
;; 2. (not (job ?x (computer programmer))) => fetch all (job ?x (computer programmer)) and remove it.

;; case 2.
;; 1. (not (job ?x (computer programmer))) => fetch all computer programmer information from database and remove from stream(but it's empty!)
;; 2. (supervisor ?x ?y) => return empty stream because ?x( and its stream of frames) is empty.

;; In logic, we interpret the statement ``not P'' to mean that P is not true.
;; In the query system, however, ``not P'' means that P is not deducible from the knowledge in the data base.


;; ex 4.64

;; before
;; (rule (outranked-by ?staff-person ?boss)
;;       (or (supervisor ?staff-person ?boss)
;;           (and (supervisor ?staff-person ?middle-manager)
;;                (outranked-by ?middle-manager ?boss))))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

(outranked-by (Bitdiddle Ben) ?who)

;; it falls to infinite loop because (and (outranked-by ... )) enters infinitely.(it matches itself)


;; ex 4.65

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(wheel ?who)

;; (wheel ?who) returns person have ?middle-manager and ?x(?middle-manager's inferior). so Bitdiddle matches. and it matches all inferiors combinated pattern.


;; ex 4.66

(sum ?amount
     (and (job ?x (computer programmer))
          (salary ?x ?amount)))

;; (accumulation-function <variable> <query pattern>)

;; <query pattern>(exactly rule) doesn't match exactly one pattern we wanted but matches all pattern include not desired(DON'T CARE). if Ben wants to use accumulation-function, he must distinct pattern produce by rule.


;; ex 4.67

;; 1. when apply rule, check if history is null
;; 2-a. if history isn't null, return empty stream.
;; 2-b. if history is null, set query to history and apply rule.

;; ex 4.68

(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
	  (append-to-form ?v ?y ?z))

(rule (reverse () ()))
(rule (reverse (?a) (?a)))
(rule (reverse (?x . ?xs) ?y)
	  (and (reverse ?xs ?z)
		   (append-to-from ?z (?x) ?y)))

;; ex 4.69

(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)


(rule (grandson ?g ?s)
	  (and (son ?g ?f)
		   (son ?f ?s)))

(rule ((grandson) ?g ?s)
	  (grandson ?g ?s))

(rule (ends-with-grandson ?x)
	  (append-to-from ?y (grandson) ?x))

(rule ((great . ?relation) ?x ?y)
	  (and
	   (ends-with-grandson ?relation) ;; check illegal relation (ex. ((great great)))
	   (?rel ?son-of-x ?y)
	   (son ?x ?son-of-x)))


;; 4.4.4 Implementing the Query System

;; additional define

(define *op-table* (make-equal-hash-table))
(define (put k1 k2 value)
  (let ((key (cons k1 k2)))
	(hash-table/put! *op-table* key value)))
(define (get k1 k2)
  (let ((key (cons k1 k2)))
	(hash-table/get *op-table* key #f)))


(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define the-empty-stream '())
(define (stream-null? s)
  (eq? s the-empty-stream))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
	   (apply proc (map stream-car argstreams))
	   (apply stream-map
			  (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
 
(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))



(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))


(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (car exp) tag)
	  #f))

;; 4.4.4.1 The Driver Loop and Instantiation

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
			   (instantiate q
							frame
                            (lambda (v f)
                              (contract-question-mark v))))
			 (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))


;; 4.4.4.2 The Evaluator

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

;; Simple queries

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;; Compound queries

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(put 'and 'qeval conjoin)


(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
	  the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(put 'or 'qeval disjoin)

;; Filters

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
		 (args exp)))


(define (always-true ignore frame-stream) frame-stream)

(put 'always-true 'qeval always-true)

;; 4.4.4.3 Finding Assersions by Pattern Matching

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
									   frame)))
		(else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))


;; Patterns with dotted tails
;; query-driver-loop processes.[primitive lisp feature]


;; 4.4.4.4 Rules and Unification

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
	(let ((unify-result
		   (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))


(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
		((var? p2) (extend-if-possible p2 p1 frame)) ; ***
		((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
								   frame)))
		 (else 'failed)))


(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
		   (unify-match
			(binding-value binding) val frame))
		  ((var? val) ; ***
		   (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
		  ((depends-on? val var frame) ; ***
		   'failed)
		  (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
			   true
			   (let ((b (binding-in-frame e frame)))
				 (if b
					 (tree-walk (binding-value b))
					 false))))
		   ((pair? e)
			(or (tree-walk (car e))
				(tree-walk (cdr e))))
		   (else false)))
  (tree-walk exp))


;; 4.4.4.5 Maintaining the Data Base


(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
	  (get-indexed-assertions pattern)
	  (get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
	(if s s the-empty-stream)))

(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
	  (get-indexed-rules pattern)
	  (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
	  (add-rule! assertion)
	  (add-assertion! assertion)))
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
	(set! THE-ASSERTIONS
		  (cons-stream assertion old-assertions))
	'ok))
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
	(set! THE-RULES (cons-stream rule old-rules))
	'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
	  (let ((key (index-key-of assertion)))
		(let ((current-assertion-stream
			   (get-stream key 'assertion-stream)))
		  (put key
			   'assertion-stream
			   (cons-stream assertion
							current-assertion-stream))))))
(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
	(if (indexable? pattern)
		(let ((key (index-key-of pattern)))
		  (let ((current-rule-stream
				 (get-stream key 'rule-stream)))
			(put key
				 'rule-stream
				 (cons-stream rule
							  current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
	  (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
	(if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;; ex 4.70

;; (define (add-assertion! assertion)
;;   (store-assertion-in-index assertion)
;;   (let ((old-assertions THE-ASSERTIONS))
;; 	   (set! THE-ASSERTIONS
;;           (cons-stream assertion old-assertions))
;; 	'ok))

;; (define (add-assertion! assertion)
;;   (store-assertion-in-index assertion)
;;   (set! THE-ASSERTIONS
;;         (cons-stream assertion THE-ASSERTIONS))
;;   'ok)

;; if didn't use olad-assertions, THE-ASSERTIONS becomes infinite stream.


;; 4.4.4.6 Stream Operations

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
	  (force delayed-s2)
	  (cons-stream
	   (stream-car s1)
	   (stream-append-delayed (stream-cdr s1) delayed-s2))))
(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
	  (force delayed-s2)
	  (cons-stream
	   (stream-car s1)
	   (interleave-delayed (force delayed-s2)
						   (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))


(define (flatten-stream stream)
  (if (stream-null? stream)
	  the-empty-stream
	  (interleave-delayed
	   (stream-car stream)
	   (delay (flatten-stream (stream-cdr stream))))))


(define (singleton-stream x)
  (cons-stream x the-empty-stream))


;; 4.4.4.7 Query Syntax Procedures

(define (type exp)
  (if (pair? exp)
	  (car exp)
	  (error "Unknown expression TYPE" exp)))
(define (contents exp)
  (if (pair? exp)
	  (cdr exp)
	  (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule))
	  '(always-true)
	  (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
		 (cons (map-over-symbols proc (car exp))
			   (map-over-symbols proc (cdr exp))))
		((symbol? exp) (proc exp))
		(else exp)))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
	(if (string=? (substring chars 0 1) "?")
		(list '?
			  (string->symbol
			   (substring chars 1 (string-length chars))))
		symbol)))

(define (var? exp)
  (tagged-list? exp '?))
(define (constant-symbol? exp)
  (symbol? exp))


(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))


(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
				  (if (number? (cadr variable))
					  (string-append (symbol->string (caddr variable))
									 "-"
									 (number->string (cadr variable)))
					  (symbol->string (cadr variable))))))

;; 4.4.4.8 Frames and Bindings

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding)
  (car binding))
(define (binding-value binding)
  (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

;; for test
(query-driver-loop)
(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! 
 (rule (grandson ?g ?s)
	   (and (son ?g ?f)
			(son ?f ?s))))


;; ex 4.71
;; (define (simple-query query-pattern frame-stream)
;;   (stream-flatmap
;;    (lambda (frame)
;;      (stream-append (find-assertions query-pattern frame)
;;                     (apply-rules query-pattern frame)))
;;    frame-stream))
;; (define (disjoin disjuncts frame-stream)
;;   (if (empty-disjunction? disjuncts)
;;       the-empty-stream
;;       (interleave
;;        (qeval (first-disjunct disjuncts) frame-stream)
;;        (disjoin (rest-disjuncts disjuncts) frame-stream))))

;; If query system falls infinite loop.(see 4.4.3) Alyssa's version occurs stack overflow on evalution time.

;; (query-driver-loop)
;; (assert! (married Minnie Mickey))
;; (assert! (rule (married ?x ?y)
;;                 (married ?y ?x)))
;; (married Minie ?who)

;; ex 4.72
;; When appending, if we use simple append instead of interleave and first stream is infinite stream, we can't access second stream.

;; ex 4.73
;; same to 4.71.(4.71, 4.72, and 4.73 are related to same topic - infinite stream)

;; ex 4.74

;; a.
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
  (stream-map stream-car ;;assume emtpy or singletone (ignore rest)
			  (stream-filter (lambda (s)
							   (not (stream-null? s))) ;; filter empty stream.
							 stream)))

;; b. maybe nothing.


;; ex 4.75

(define (uniquely-asserted query frames)
  (stream-flatmap
   (lambda (frame)
	 (let ((result (qeval (car query)
						  (singleton-stream frame))))
	   (cond ((stream-null? result) the-empty-stream)
			 ((stream-null? (stream-cdr result)) result)
			 (else the-empty-stream))))
   frames))
(put 'unique 'qeval uniquely-asserted)

;; Ben's information
(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))

;; Alyssa's information
(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

;; Fect's information
(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))

;; Tweakit's information
(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

;; Reasoner's information
(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

;; Warbucks's information
(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

;; Scrooge's information
(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))

;; Cratchet's information
(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

;; Aull's information
(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))


(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))

(assert! (can-do-job (computer programmer)
					 (computer programmer trainee)))

(assert! (can-do-job (administration secretary)
					 (administration big wheel)))


(unique (job ?x (computer wizard)))

(unique (job ?x (computer programmer)))

(and (job ?x ?j)
	 (unique (job ?anyone ?j)))

;; ex 4.76

(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))
(define (empty-frame? frame) (null? frame))

(define (unify-if-possible var val frame)
  (let ((b (binding-in-frame var frame)))
    (cond ((not b) (extend var val frame))
          ((equal? (binding-value b) val) frame)
          ((var? val)
           (unify-if-possible val
                              (binding-value b)
                              frame))
          (else 'failed))))

(define (unify-frames f1 f2)
  (cond ((empty-frame? f1) f2)
        ((eq? f2 'failed) 'failed)
        (else
         (let ((b (first-binding f1)))
           (unify-frames
            (rest-bindings f1)
            (unify-if-possible (binding-variable b)
                               (binding-value b)
                               f2))))))

(define (unify-frame-streams fs1 fs2)
  (stream-flatmap
   (lambda (f1)
     (stream-filter
      (lambda (f) (not (eq? f 'failed)))
      (stream-map (lambda (f2) (unify-frames f1 f2))
                  fs2)))
   fs1))



