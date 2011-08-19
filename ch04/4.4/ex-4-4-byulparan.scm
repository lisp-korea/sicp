
;; 4.4 논리로 프로그램 짜기

;; 4.4.1 연역식 정보 찾기

;; 컴퓨터 부서
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (Computer wizard))
(salary (Bitdiddle Ben) 60000)
(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))



;; 관리자
(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)


(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))


;; 회계부서 
(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))


;; ===========================================
;; 간단한 쿼리

;; Query input :
(job ?x (computer programmer))

;; Query results:
(job (Hacker Alyssa P) (computer programmer))
(job (Fect Cy D) (computer programmer))


;; Query input :
(job ?x (computer ?type))

;; Query results :
(job (Bitdiddle Ben) (computer wizard))
(job (Hacker Alyssa P) (computer programmer))
(job (Fect Cy D) (computer programmer))
(job (Tweakit Lem E) (computer technician))


;; Query input :
(job ?x (computer . ?type))

;; Query results :
(job ..  (computer programmer trainee))
(job ..  (computer programmer))
(job ..  (computer))


;; 연습문제 4.55

;; a. Ben Bitdiddle 이 관리하는 모든 사람
(supervisor ?x (Bitdiddle Ben)
;; Query results :
(supervisor (Tweakit Lem E) (Bitdiddle Ben))
(supervisor (Fect Cy D) (Bitdiddle Ben))
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

;; b. 회계 부서 사람의 이름과 직업
(job ?x (accounting . ?y))

;; c. 슬러머빌에 사는 모든 사람의 이름과 주소
(address ?x (Slumerville . ?y))


;; 합친 쿼리

;; 모든 컴퓨터 프로그래머의 주소를 찾아내는 쿼리
(and (job ?person (computer programmer))
     (address ?person ?where))


;; 컴퓨터 부서이거나 
(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Alyssa P)))


(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))

;; 연습문제 4.56

;; a. Ben Bitdiddle 이 관리하는 모든 사람의 이름과 주소

(and (supervisor ?name (Bitdiddle Ben))
     (address ?name ?address))

;; b. Ben 보다 급여를 적게 받는 사람, 아울러 그 급여와 Ben 의 월급
(and (salary (Bitdiddle Ben) ?salary-of-Ben)
     (salary ?name  ?salary)
     (lisp-value > ?salary-of-Ben ?salary))

;; c. 컴퓨터 부서에 속하지 않는 사람이 관리하는 모든 사람, 아울러 그 관리자의 이름과 직업

(and (not (job ?boss (computer . ?a)))
     (supervisor ?name  ?boss)
     (job ?boss ?job))


;; 규칙
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
	   (address ?person-2 (?town . ?rest-2))
	   (not (same ?person-1 ?person-2))))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
	   (supervisor ?x ?middle-manager)))


;; Query input :
(lives-near ?x (Bitdiddle Ben))

;; Query results :
(lives-near (Reasoner Louis) (Bitdiddle Ben))
(lives-near (Aull DeWitt) (Bitdiddle Ben))
     

;; Ben 과 이웃에 사는 모든 프로그래머를 찾는 쿼리
(and (job ?x (computer programmer))
     (lives-near ?x (Bitdiddle Ben)))



(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
	  (and (supervisor ?staff-person ?middle-manager)
	       (outrankd-by ?middle-manager ?boss))))


;; 연습문제 4.57

(rule (can-replace ?person-1 ?person-2)
      (and (not (same ?person-1 ?person-2))
	   (job ?person-1 ?job-1)
	   (job ?person-2 ?job-2)
	   (or (same ?job-1 ?job-2)
	       (can-do-job ?job-1 ?job-2))))

;; a.
(can-replace ?who (Fect Cy D))

;; b.
(and (salary ?x ?x-salary)
     (salary ?y ?y-salary)
     (lisp-value > ?y-salary ?x-salary)
     (can-replace ?y-salary ?x-salary))


;; 연습문제 4.58

(rule (bigshot ?person)
      (and (job ?person (?job . ?x))
	   (or
	    (not (supervisor ?person ?boss))
	    (not (job ?boss (?job . ?y))))))

;; 연습문제 4.59

(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wendnesday 3pm))
(meeting administration (Friday 1pm))

(meeting whole-company (Wednesday 4pm))


;; a. 금요일에 있는 모든 회의를 찾는 쿼리
(meetring ?x (Friday . ?y))

;; b. 한 사람이 참석해야 할 회사 전체 회의와 부서별 회의를 찾는 규칙을 만들자

(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
	  (and
	   (job ?person (?div . ?x))
	   (meeting ?div ?day-and-time))))

;; c.
(and (meeting-time (Hacker Alyssa P) (Wendnesday ?time))
     (meeting ?job (Wendnesday ?time)))


;; 연습문제 4.60



;; 프로그램으로서의 논리

(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-from ?v ?u ?z))


;; 연습문제4.61

(rule (?x next-to ?y in (?x ?y . ?u)))

(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

;; input :
(?x next-to ?y in (1 (2 3) 4))

(?x next-to 1 in (2 1 3 1))
	  

;; 연습문제 4.62

(rule (last-pair (?x . ())      (?x)))

(rule (last-pair (?y . ?z) (?x))
      (last-pair ?z (?x)))


;; 연습문제 4.63


(rule (grandson ?s ?g)
       (and (son ?f ?s)
            (son ?g ?f)))

(rule (son-of ?s ?m)
      (or (son ?m ?s)
          (and (wife ?m ?w)
               (son ?w ?s))))



