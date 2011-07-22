

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
;; ex 4.44
