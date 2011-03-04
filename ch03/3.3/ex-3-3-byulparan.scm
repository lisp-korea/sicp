
;; racket -l r5rs/run

;; 연습문제 3.12

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)

(define w (append! x y))
w
(cdr x)


;; 연습문제 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; what happen?
(last-pair z) 				


;; 연습문제 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x) y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))


(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1
(set-to-wow! z1)
z2
(set-to-wow! z2)

;; 연습문제 3.15

;; 연습문제 3.16

(define (count-pairs x)
  (if (not (pair? x)) 0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x)) 1)))

;; (define (count-pairs x)
;;   (if (not (pair? x)) (begin
;; 			(display x) (display "\n")
;; 			0)
;;       (+ (count-pairs (car x))
;; 	 (count-pairs (cdr x)) 1)))

;; car 과 cdr 이 같은 pair 를 가리키고 있다면?!

;; 세계의 pair 가 있지만..4를 출력.
(let ((a (list 'a)))
  (let ((b (list a a)))
    (count-pairs b)))

;; 7
(let ((a (cons 10 20)))
  (let ((b (cons a a)))
    (let ((c (cons b b)))
      (count-pairs c))))

;; 영원히 도는 프로시져
(let ((a (list 1 2)))
  (let ((b (cons 10 a)))
    (set-cdr! (cdr a) b)
    (count-pairs a)))

;; 연습문제 3.17

(define (find-item item seq)
  (cond ((null? seq) #f)
	((eq? item (car seq)) #t)
	(else (find-item item (cdr seq)))))

;; (let ((a '(1 2 3)))
;;   (let ((b (list 10 20 a 30)))
;;     (find-item a b)))

(define (new-count-pairs x)
  (let ((repo '()))
    (define (count-pairs x)
      (if (or (not (pair? x)) (find-item x repo)) 0
	  (begin
	    (set! repo (cons x repo))
	    (+ (count-pairs (car x))
	       (count-pairs (cdr x)) 1))))
    (count-pairs x)))



(let ((a (list 'a)))
  (let ((b (list a a)))
    (new-count-pairs b)))


(let ((a (cons 10 20)))
  (let ((b (cons a a)))
    (let ((c (cons b b)))
      (new-count-pairs c))))

(let ((a (list 1 2)))
  (let ((b (cons 10 a)))
    (set-cdr! (cdr a) b)
    (new-count-pairs a)))


;; 연습문제 3.18

(define (make-cycle list)
  (set-cdr! (last-pair list) list)
  list)

(let ((a (make-cycle '(1 2 3))))
  (eq? a (cdddr a)))

(define (find-cycle list)
  (let ((copy '()))
    (define (inner-find-cycle x)
      (cond ((null? x) #f)
	    ((find-item x copy) #t)
	    (else (begin
		    (set! copy (cons x copy))
		    (inner-find-cycle (cdr x))))))
    (inner-find-cycle (cdr list))))

(find-cycle '(1 2 3 4))
(find-cycle (make-cycle '(1 2 3 4)))
(find-cycle (cons 'q (make-cycle '(1 2 3 4))))

;; 연습문제 3.19
;; Floyd's idea:
 (define (contains-cycle? lst) 
   (define (safe-cdr l) 
     (if (pair? l) 
         (cdr l) 
         '())) 
   (define (iter a b) 
     (cond ((not (pair? a)) #f) 
           ((not (pair? b)) #f) 
           ((eq? a b) #t) 
           ((eq? a (safe-cdr b)) #t) 
           (else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))))) 
   (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))

(contains-cycle? (make-cycle '(1 2 3 4)))

;; 연습문제 3.20

(define (cons2 x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  ((eq? m 'set-car!) set-x!)
	  ((eq? m 'set-cdr!) set-y!)
	  (else (error "Undefined operation --CONS"))))
  dispatch)

(define (car2 z)
  (z 'car))

(define (cdr2 z)
  (z 'cdr))

(define (set-car2! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr2! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define x (cons2 1 2))
(define z (cons2 x x))
(set-car2! (cdr2 z) 17)

(car2 x)


;; 3.3.2 큐

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue) (begin
				  (set-front-ptr! queue new-pair)
				  (set-rear-ptr! queue new-pair)
				  queue))
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue) (error "DELETE! called with an empty queue"))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))


;; 연습문제 3.21

(define (error message)
  (display message)
  (display "\n"))

(define q1 (make-queue))

(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)
(empty-queue? q1)

(define (print-queue queue)
  (front-ptr queue))

(print-queue q1)
	   
	   

;; 연습문제 3.22

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with empty queue")
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?) (begin
				(set! front-ptr new-pair)
				(set! rear-ptr new-pair)))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?) (error "DELETE! called on emtpy queue"))
	    (else
	     (set! front-ptr (cdr front-ptr)))))
    (define (print-queue)
      front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    ((eq? m 'front-queue) front-queue)
	    ((eq? m 'print-queue) print-queue)
	    (else (error "ERR"))))
    dispatch))


;; 연습문제 3.23
;; deque

(define (make-deque)
  (let ((deque (cons '() '())))
    (define (front-ptr)
      (car deque))
    (define (rear-ptr)
      (cdr deque))
    (define (set-front-ptr! item)
      (set-car! deque item))
    (define (set-rear-ptr! item)
      (set-cdr! deque item))
    (define (empty-deque?)
      (null? (front-ptr)))
    (define (front-deque)
      (cond ((empty-deque?) (error "FRONT called with an empty deque"))
	    (else (caar (front-ptr)))))
    (define (rear-deque)
      (cond ((empty-deque?) (error "REAR called with an empty deque"))
	    (else (caar (rear-ptr)))))
    (define (front-insert-deque! item)
      (let ((new-pair (cons (cons item '()) '())))
	(cond ((empty-deque?) (begin
				(set-front-ptr! new-pair)
				(set-rear-ptr! new-pair)))
	      (else
	       (set-cdr! new-pair (front-ptr))
	       (set-cdr! (car (front-ptr)) new-pair)
	       (set-front-ptr! new-pair)))))
    (define (rear-insert-deque! item)
      (let ((new-pair (cons (cons item '()) '())))
	(cond ((empty-deque?) (begin
			       (set-front-ptr! new-pair)
			       (set-rear-ptr! new-pair)))
	      (else
	       (set-cdr! (rear-ptr) new-pair)
	       (set-cdr! (car new-pair) (rear-ptr))
	       (set-rear-ptr! new-pair)))))
    (define (front-delete-deque!)
      (cond ((empty-deque?) (error "FRONT-DELETE called with on empty deque"))
	    ((eq? (front-ptr) (rear-ptr))
	     (set-front-ptr! '())
	     (set-rear-ptr! '()))
	    (else
	     (set-front-ptr! (cdr (front-ptr)))
	     (set-cdr! (car (front-ptr)) '()))))
    (define (rear-delete-deque!)
      (cond ((empty-deque?) (error "REAR-DELETE called with on emtpy deque"))
	    ((eq? (front-ptr) (rear-ptr))
	     (set-front-ptr! '())
	     (set-rear-ptr! '()))
	    (else
	     (set-rear-ptr! (cdar (rear-ptr)))
	     (set-cdr! (rear-ptr) '()))))
    (define (print-deque)
      (define (print-deque-inner deque acc)
	(cond ((null? deque) acc)
	      (else (print-deque-inner (cdr deque)
				       (append acc (list (caar deque)))))))
      (print-deque-inner (front-ptr) '()))
    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) empty-deque?)
	    ((eq? m 'front-deque) front-deque)
	    ((eq? m 'rear-deque) rear-deque)
	    ((eq? m 'front-insert-deque!) front-insert-deque!)
	    ((eq? m 'front-delete-deque!) front-delete-deque!)
	    ((eq? m 'rear-insert-deque!) rear-insert-deque!)
	    ((eq? m 'rear-delete-deque!) rear-delete-deque!)
	    ((eq? m 'print-deque) print-deque)
	    (else (error "ERROR"))))
    dispatch))



;; 3.3.3
;; 표

(define (lookup key table)
  (let ((record (byul-assoc key (cdr table))))
    (if record
	(cdr record)
	#f)))

(define (byul-assoc key records)
  (cond ((null? records) #f)
	((equal? key (caar records)) (car records))
	(else (byul-assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table (cons (cons key value) (cdr table)))))
  'ok!)


(define (make-table)
  (list '*table*))


;; 이차원 표

(define (lookup key-1 key-2 table)
  (let ((subtable (byul-assoc key-1 (cdr table))))
    (if subtable
	(let ((record (byul-assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      #f))
	#f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (byul-assoc key-1 (cdr table))))
    (if subtable
	(let ((record (byul-assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable (cons (cons key-2 value)
				       (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  'ok)


;; 프로시저 속에 표 감추기

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (byul-assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (byul-assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (byul-assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (byul-assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable (cons (cons key-2 value)
					   (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation --TABLE"))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'letters 'a 97)
(put 'letters 'b 98)
(get 'letters 'b)
(get 'letters 'a)



;; 연습문제 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (byul-assoc key records)
      (cond ((null? records) #f)
	    ((same-key? key (caar records)) (car records))
	    (else (byul-assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (byul-assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (byul-assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (byul-assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (byul-assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable (cons (cons key-2 value)
					   (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation --TABLE"))))
    dispatch))

(define table1 (make-table equal?))
((table1 'insert-proc!) 'a 100 10)
((table1 'insert-proc!) 'a 101 20)
((table1 'lookup-proc) 'a 100.2)
((table1 'lookup-proc) 'a 101.2)


(define table2 (make-table (lambda (a b)
			     (cond ((number? a) (if (> 0.5 (abs (- a b))) #t #f))
				   (else (equal? a b))))))

((table2 'insert-proc!) 'a 100 10)
((table2 'insert-proc!) 'a 101 20)
((table2 'lookup-proc) 'a 100.2)
((table2 'lookup-proc) 'a 100.8)


;; 연습문제 3.25
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (byul-assoc key records)
      (cond ((null? records) #f)
	    ((same-key? key (caar records)) (car records))
	    (else (byul-assoc key (cdr records)))))
    (define (lookup key-list)
      (define (look-inner key table)
	(let ((subtable (byul-assoc (car key) (cdr table))))
	  (if subtable
	      (cond ((null? (cdr key)) (cdr subtable))
		    (else (look-inner (cdr key) subtable)))
	      #f)))
      (look-inner key-list local-table))
    (define (insert! key-list value)
      (define (insert-inner key table)
      (let ((subtable (byul-assoc (car key) (cdr table))))
	(if subtable
	    (cond ((null? (cdr key))  (set-cdr! subtable value))
		  (else (insert-inner (cdr key) subtable)))
	    (begin
	      (if (null? (cdr key)) (set-cdr! table (cons
						     (cons  (car key) value) (cdr table)))
		  (let ((new-subtable (list (car key))))
		    (set-cdr! table (cons new-subtable (cdr table)))
		    (insert-inner (cdr key) new-subtable)))))))
      (insert-inner key-list local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation --TABLE"))))
    dispatch))

(define table (make-table equal?))


((table 'lookup-proc) '(a c))

((table 'insert-proc!) '(a b c) 2110)
((table 'insert-proc!) '(a b d) 20)
((table 'insert-proc!) '(a b) 80)
((table 'insert-proc!) '(a c) 80)
((table 'insert-proc!) '(a d e f g) 10)
((table 'insert-proc!) '(a d e g h) 40)
((table 'lookup-proc) '(a d e))
((table 'lookup-proc) '(a d e f g))
((table 'lookup-proc) '(a d e g h))


;; 연습문제 3.26

;; 연습문제 3.27

(define (lookup key table)
  (let ((record (byul-assoc key (cdr table))))
    (if record
	(cdr record)
	#f)))

(define (byul-assoc key records)
  (cond ((null? records) #f)
	((equal? key (caar records)) (car records))
	(else (byul-assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table (cons (cons key value) (cdr table)))))
  'ok!)


(define (make-table)
  (list '*table*))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
	(or previously-computed-result
	    (let ((result (f x)))
	      (insert! x result table)
	      result))))))

(define memo-fib
  (memoize (lambda (n)
	     (cond ((= n 0) 0)
		   ((= n 1) 1)
		   (else (+ (memo-fib (- n 1))
			    (memo-fib (- n 2))))))))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

