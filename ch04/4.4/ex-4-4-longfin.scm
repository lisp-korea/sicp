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