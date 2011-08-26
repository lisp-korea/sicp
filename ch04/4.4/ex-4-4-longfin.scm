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

