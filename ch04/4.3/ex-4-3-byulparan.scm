

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
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (a-pythagorean-triple-between low)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
	(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))

(a-pythagorean-triple-between 10)

;; 무한 루프.....
	
;; ex 4.37

;; ver. ben bitdiddle 

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
	(hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
	(require (>= hsq ksq))
	(let ((k (sqrt ksq)))
	  (require (integer? k))
	  (list i j k))))))





(define (distinct? items)
  (cond ((null? items) #t)
	((null? (cdr items)) #t)
	((member (car items) (cdr items)) #f)
	(else (distinct? (cdr items)))))

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
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))

((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))

;; ex 4.39
;; 답은 달라지지 않는다. 하지만 수행시간은??

(define (md2)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 1)))
    (require (not (= fletcher 5)))
    (require (not (= baker 5)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))


;; ex 4.40

(define (md3)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require
            (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith)))))))


;; ex 4.41 
(define *all-available*
  (let ((collect '()))
    (for-each (lambda (x)
		(for-each (lambda (y)
			    (for-each (lambda (z)
					(for-each (lambda (l)
						    (for-each (lambda (k)
								(set! collect
								      (append collect
									      (list (list (cons 'baker x) (cons 'cooper y) (cons 'fletcher z) (cons 'miller l) (cons 'smith k))))))
							      '(1 2 3 4 5)))
						  '(1 2 3 4 5)))
				      '(1 2 3 4 5)))
			  '(1 2 3 4 5)))
	      '(1 2 3 4 5))
    collect))

(define (find-man name list)
  (cdr (assoc name list)))

(define (my-distrinct? items)
  (cond ((null? items) #t)
	((null? (cdr items)) #t)
	((member (cdr (car items)) (map cdr (cdr items))) #f)
	(else (my-distrinct? (cdr items)))))


(define (search-md list)
  (if (and (my-distrinct? (car list))
	   (not (= (find-man 'baker (car list)) 5))
	   (not (= (find-man 'cooper (car list)) 1))
	   (not (= (find-man 'fletcher (car list)) 5))
	   (not (= (find-man 'fletcher (car list)) 1))
	   (> (find-man 'miller (car list)) (find-man 'cooper (car list)))
	   (not (= (abs (- (find-man 'smith (car list)) (find-man 'fletcher (car list)))) 1))
	   (not (= (abs (- (find-man 'fletcher (car list)) (find-man 'cooper (car list)))) 1)))
      (car list)
      (search-md (cdr list))))

(search-md *all-available*)

;; ex 4.42

(define (distinct? items)
  (cond ((null? items) #t)
	((null? (cdr items)) #t)
	((member (car items) (cdr items)) #f)
	(else (distinct? (cdr items)))))

(define (xor a b)
  (and (or a b)
       (not (and a b))))

(define (liars)
    (let ((betty (amb 1 2 3 4 5))
          (ethel (amb 1 2 3 4 5))
          (joan (amb 1 2 3 4 5))
          (kitty (amb 1 2 3 4 5))
          (mary (amb 1 2 3 4 5)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
	  (list 'ethel ethel)
	  (list 'joan joan)
	  (list 'kitty kitty)
	  (list 'mary mary))))
     
;; ex 4.43
(define (yachts)
  (let ((gabrielle (amb 'moore 'downing 'hall 'barnacle 'parker))
	(lorna (amb 'moore 'downing 'hall 'barnacle 'parker))
	(rosalind (amb 'moore 'downing 'hall 'barnacle 'parker))
	(melissa (amb 'moore 'downing 'hall 'barnacle 'parker))
	(maryann (amb 'moore 'downing 'hall 'barnacle 'parker)))
    (require (not (eq? gabrielle 'barnacle)))
    (require (not (eq? lorna 'moore)))
    (require (not (eq? rosalind 'hall)))
    (require (eq? melissa 'barnacle))
    (require (eq? maryann 'moore))
    (require
     (cond ((eq? gabrielle 'moore) (eq? lorna 'parker))
	   ((eq? gabrielle 'downing) (eq? melissa 'parker))
	   ((eq? gabrielle 'hall) (eq? rosalind 'parker))
	   (else false)))
    (require
     (distinct? (list gabrielle lorna rosalind melissa maryann)))
    (list (list 'gabrielle gabrielle)
	  (list 'lorna lorna)
	  (list 'rosalind rosalind)
	  (list 'melissa melissa)
	  (list 'maryann maryann))))

;; ex 4.44


;; 자연어 문법 분석

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

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
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


;(parse '(the student with the cat sleeps in the class))

;; ex 4.45


;; ex 4.46

;; ex 4.47

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
	     (parse-verb-phrase)
	     (parse-prepositional-phrase))))

;; 무한 룹??

;; ex 4.48
(define adjectives '(adjective gray tired nice))

(define (parse-simple-noun-phrase)
  (amb
   (list 'simple-noun-phrase
	 (parse-word articles)
	 (parse-word nouns))
   (list 'adjective-noun-phrase
	 (parse-word articles)
	 (parse-word adjectives)
	 (parse-word nouns))))

(parse '(The tired professor lectures to a gray cat))


;; ex 4.49

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (let ((found-word (random-list-element (cdr word-list))))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (nth i lst)
  (cond ((null? lst) '())
	((= i 0) (car lst))
	(else (nth (- i 1) (cdr lst)))))

(define (random-list-element lst)
  (nth (random (length lst)) lst))

(parse '(The professor lectures))
