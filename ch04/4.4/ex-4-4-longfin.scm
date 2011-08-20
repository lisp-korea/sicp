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


				 
	  
