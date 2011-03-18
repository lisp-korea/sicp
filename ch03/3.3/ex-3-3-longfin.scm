(define (error reason . args)
      (display "Error: ")
      (display reason)
      (for-each (lambda (arg) 
                  (display " ")
		  (write arg))
		args)
      (newline)
      (scheme-report-environment -1))

;; 3.3.1 Mutable List Structure

(define (ncons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))


;; ex 3.12

(define (nappend x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
;; [[a] [+]
;;        +-[[b] [nil]]
(define y (list 'c 'd))
;; [[c] [+]
;;        +-[[d] [nil]]
(define z (nappend x y))
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [+]]
;;                                +-[[d] [nil]]

(cdr x)
;;(b)

(define w (append! x y))
;; (last-pair x)
;; [[b] [nil]]

;;(set-cdr! (last-pair x) y)
;; [[b] [+]]
;;        +=> y

;; x
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [+]]
;;                                +-[[d] [nil]]

(cdr x)
;; [[b] [+]]
;;        +-[[c] [+]]
;;                   +-[[d] [nil]]

;; ex 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; (list 'a 'b 'c) => x
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [nil]]

;; (set-cdr! (last-pair x) x)
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [+]]
;;                                +=> x

;; z is infinite sequence.

;; ex 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

;; (list 'a 'b 'c 'd) => x1
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [+]]
;;                               +-[[d] [nil]]

;; '() => y1
;; nil

;; (cdr x1) => temp1
;; [[b] [+]]
;;        +-[[c] [+]]
;;                   +-[[d] [nil]]

;; (set-cdr! x1 y1) => x1'
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [+]]
;;                               +-[[d] [nil]]

;; temp1 => x2
;; x1' => y2

;; (cdr x2) => temp2
;; [[c] [+]]
;;        +-[[d] [nil]]

;; (set-cdr! x2 y2) => x2'
;; [[b] [+]]
;;        +-[[c] [+]]
;;                   +-[[d] [+]]
;;                               +- y2

;; temp2 => x3
;; x2' => y3
;; ....

;; mystery reverses sequence.

;; v : (a b c d)
;; w : (d c b a)


;; Sharing and identity

(define x (list 'a 'b))
;; [[a] [+]]
;;        +-[[b] [nil]]

(define z1 (cons x x))

;; [[+] [+]]
;;   +   +--+ 
;;   +-----+- x

(define z2 (cons (list 'a 'b) (list 'a 'b)))
;; [[+] [+]]
;;   +   +-[[a] [+]]
;;   +              +-[[b] [nil]]
;;   +-[[a] [+]]
;;              +-[[b] [nil]]

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)
(set-to-wow! z1)
(set-to-wow! z2)

;; ex 3.15

;; z1
;; [[+] [+]]
;;   +   +--+ 
;;   +-----+- [[wow] [+]]
;;                                 +-[[b] [nil]]

;; z2
;; [[+] [+]]
;;   +   +-[[a] [+]]
;;   +              +-[[b] [nil]]
;;   +-[[wow] [+]]
;;                    +-[[b] [nil]]

;; ex 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(define count3 '(1 2 3))
;; [[1] [+]]
;;        +-[[2] [+]]
;;                    +-[[3] [nil]]
(count-pairs count3)

(define p (cons 1 2))
;; [[1] [+]]
;;        +-[[p] [nil]]
(define count4 (cons p (cons p 4)))
;; [[+] [+]]
;;   +   +-[[+] [4]]
;;   +          +   
;;   +          +-[[1] [2]
;;   +-[[1] [2]]
(count-pairs count4)

(define p2 (cons p p))
;; [[+] [+]]
;;   ++ +-[[1] [2]]
(define count7 (cons p2 p2))
;; [[+] [+]]
;;   +++
;;        +-[[+] [+]]
;;               +++-[[1] [2]]
(count-pairs count7)

(define counti (cons p p))
(set-cdr! counti counti)

;; [[+] [+]] => counti
;;   +   +-counti
;;   +-[[1] [2]]

;; ex 3.17

(define (contains? list x)
  (cond
   ((null? list) #f)
   ((eq? x (car list)) #t)
   (else (contains? (cdr list) x))))
(define (count-pairs x)
  (let ((founded '()))
    (define (count-pairs-inner x)
      (cond
       ((not (pair? x)) 0)
       ((contains? founded x) 0)
       (else
	(set! founded (cons x founded))
	(+
	 (count-pairs-inner (car x))
	 (count-pairs-inner (cdr x))
	 1))))
    (count-pairs-inner x)))

;; ex 3.18

(define (cycle? x)
  (let ((founded '()))
    (define (inner x)
      (cond
       ((not (pair? x)) #f)
       ((null? x) #f)
       ((contains? founded x) #t)
       (else
	(set! founded (cons x founded))
	(inner (cdr x)))))
    (inner x)))

(define x (cons 1 2))
(cycle? x)
;; #f
(set-cdr! x x)
(cycle? x)
;; #t

(define y (cons 3 x))
(set-cdr! x y)
(cycle? x)
;;#t

(set-car! x x)
(cycle? x)
;;#t

(define z (cons x 3))
(set-car! x z)
(cycle? x)
;;#t

;; ex 3.19

(define (cycle? x)
  (define (safe-cdr x)
    (if (pair? x)
	(cdr x)
	'()))
  (define (iter a b)
    (cond
     ((not (pair? a)) #f)
     ((not (pair? b)) #f)
     ((eq? a b) #t)
     ((eq? a (safe-cdr b)) #t)
     (else (iter (safe-cdr a)
		 (safe-cdr (safe-cdr b))))))
  (iter (safe-cdr x) (safe-cdr (safe-cdr x))))

;; 2nd and 4th
;; 2nd and 5th

;; 3nd and 6th
;; 3nd and 7th

;; ...


;; Mutation is just assignment

(define (ncons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  (else (error "Undefined opertion -- CONS" m))))
  dispatch)

(define (ncar z) (z 'car))

(define (ncdr z) (z 'cdr))


(define (ncons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  ((eq? m 'set-car!) set-x!)
	  ((eq? m 'set-cdr!) set-y!)
	  (else (error "Undefined opertaion -- CONS" m))))
  dispatch)

(define (ncar z) (z 'car))
(define (ncdr z) (z 'cdr))
(define (nset-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (nset-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;; ex 3.20

(define x (ncons 1 2))
;; G = [ncons=(...), ncar=(...), ncdr=(...), x=(E1.dispatch)]
;; E1 = [x=1, y=2,dispatch=(...)]

(define z (ncons x x))
;; G = [ncons=(...), ncar=(...), ncdr=(...), x=(E1.dispatch), z=(E2.dispatch)]
;; E1 = [x=1, y=2,dispatch=(...)]
;; E2 = [x=G.x, y=G.x, dispatch=(...)]

(nset-car! (ncdr z) 17)
(ncar x)
;; G = [ncons=(...), ncar=(...), ncdr=(...), x=(E1.dispatch), z=(E2.dispatch)]
;; E1 = [x=17, y=2,dispatch=(...)]
;; E2 = [x=G.x, y=G.x, dispatch=(...)]


;; 3.3.2 Representing Queues

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))

;; ex 3.21

(define q1 (make-queue))
;; (())
(insert-queue! q1 'a)
;; ((a) a)
(insert-queue! q1 'b)
;; ((a b) b)
(delete-queue! q1)
;; ((b) b)
(delete-queue! q1)
;; (() b)

(define (print-queue queue)
  (display (front-ptr queue))
  (display (newline)))

;; ex 3.22

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front)
      (if (empty?)
	  (error "FRONT called with an empty queue")
	  (car front-ptr)))
    (define (insert! item)
      (let ((new-pair (cons item '())))
	(cond ((empty?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair)
	       dispatch)
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)
	       dispatch))))
    (define (delete!)
      (cond ((empty?)
	     (error "DELETE! called with an empty queue"))
	    (else
	     (set! front-ptr (cdr front-ptr))
	     dispatch)))
    (define (print)
      (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'insert!) insert!)
	    ((eq? m 'delete!) delete!)
	    ((eq? m 'empty?) empty?)
	    ((eq? m 'front) front)
	    ((eq? m 'display) print)
	    (else
	     (error "Unknown operation"))))
    dispatch))

(define q1 (make-queue))
((q1 'insert!) 'a)
((q1 'insert!) 'b)

((q1 'display))
;;(a b)
((q1 'delete!))

((q1 'display))
;;(b)


;; ex 3.23

;; constructor
(define (make-deque)
  (cons '() '()))

;; utility
(define (front-ptr deque)
  (car deque))
(define (rear-ptr deque)
  (cdr deque))
(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))
(define (set-prev-link! item link)
  (set-car! (cdr item) link))
(define (set-next-link! item link)
  (set-cdr! (cdr item) (cons link '())))
(define (prev-link item)
  (cadr item))
(define (next-link item)
  (caddr item))
(define (get-value item)
  (car item))
  
  
;; predicate
(define (empty-deque? deque)
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))

;;selectors
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT-DEQUE called with an empty deque")
      (get-value (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR-DEQUE called with an empty")
      (get-value (rear-ptr deque))))

;; mutators
(define (front-insert-deque! deque item)
  (let ((new-pair (list item '() '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	   (set-next-link! new-pair (front-ptr deque))
	   (set-prev-link! (front-ptr deque) new-pair)
	   (set-front-ptr! deque new-pair)))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (list item '() '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	   (set-next-link! (rear-ptr deque) new-pair)
	   (set-prev-link! new-pair (rear-ptr deque))
	   (set-rear-ptr! deque new-pair)))))
(define (front-delete-deque! deque)
  (cond ((empty-queue? deque)
	 (error "FRONT-DELETE called with an empty deque"))
	 (else
	  (let ((new-front (next-link (front-ptr deque))))
	    (set-front-ptr! deque new-front)
	    (if (not (null? new-front))
		(set-prev-link! new-front '())
		(set-rear-ptr! deque new-front))))))
(define (rear-delete-deque! deque)
  (cond ((empty-queue? deque)
	 (error "REAR-DELETE called with an empty deque"))
	(else
	 (let ((new-rear (prev-link (rear-ptr deque))))
	   (set-rear-ptr! deque new-rear)
	   (if (not (null? new-rear))
	       (set-next-link! new-rear '())
	       (set-prev-link! deque new-rear))))))

;; 3.3.3 Representing Tables


(define (lookup key table)
  (let ((record (nassoc key (cdr table))))
    (if record
	(cdr record)
	#f)))

(define (nassoc key records)
  (cond ((null? records) #f)
	((equal? key (caar records)) (car records))
	(else (nassoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (nassoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr table
		 (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))

;; Two-dimensional tables

(define (lookup key-1 key-2 table)
  (let ((subtable (nassoc key-1 (cdr table))))
    (if subtable
	(let ((record (nassoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      #f))
	#f)))


(define (insert! key-1 key-2 value table)
  (let ((subtable (nassoc key-1 (cdr table))))
    (if subtable
	(let ((record (nassoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key-2 value)
			      (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  'ok)

;; Creating local tables

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


;; ex 3.24

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
	    ((same-key? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; ex 3.25

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (lookup-iter keys local-table))
    (define (lookup-iter keys table)
      (let ((key (car keys)))
	(if (null? key)
	    #f
	    (let ((subtable (assoc key  (cdr table))))
	      (if subtable
		  (if (and (list? subtable)
			   (not (null? (cdr keys))))
		      (lookup-iter (cdr keys) subtable)
		      (cdr subtable))
		  #f)))))
    (define (insert! keys value)
	(if (null? keys)
	    (error "given key is empty!")
	    (insert-iter! keys value local-table)))
    (define (insert-iter! keys value table)
      (let ((key (car keys)))
	(let ((subtable (assoc key (cdr table))))
	  (if (null? (cdr keys))
	      (if subtable
		  (set-cdr! subtable value)
		  (set-cdr! table
			    (cons (cons key value) (cdr table))))
	      (cond
	       ((not subtable)
		(set-cdr! table (cons (list key) (cdr table)))
		(insert-iter! keys value table))
	       ((list? subtable)
		(insert-iter! (cdr keys) value subtable))
	       (else
		(set-cdr! subtable (list key))
		(insert-iter! (cdr keys) value subtable))))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))



;; 3.3.4 A Simulator for Digital Circuits


(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

;; a or b -> d
(or-gate a b d)

;; a and b -> c
(and-gate a b c)

;; not c -> e
(inverter c e)

;; d and e -> s
(and-gate d e s)

(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


;; Primitive function boxes

;; (get-signal <WIRE>)
;; (set-signal! <WIRE> <NEW VALUE>)
;; (add-action! <WIRE> <PROCEDURE OF NO ARGUMENTS>)

(define (inverter input output)
  (define (inverter-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! input inverter-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
	   (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define logical-and *)

;; ex 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (if (= (+ a b) 0) 0 1))
	

;; ex 3.29

"
        a1	a2	|	output
	0	0	|	0
	0	1	|	1
	1	0	|	1
	1     	1	|	1
	a or b = not(not (a) and not (b))
"

(define (or-gate a1 a2 output)
  (let ((a (make-wire))
	(b (make-wire))
	(c (make-wire)))
    (begin
      (inverter a1 a)
      (inverter a2 b)
      (and-gate a b c)
      (inverter c output))))


;; ex 3.30

(define (ripple-carry as bs ss c)
  (cond ((not (= (length as) (length bs) (length ss)))
	 (error "Invalid input"))
	((= (length ss) 0) 'ok)
	(else
	 (begin
	   (let ((nc (make-wire)))
	     (full-adder (car as) (car bs) c (car ss) nc)
	     (ripple-carry
	      (cdr as)
	      (cdr bs)
	      (cdr ss)
	      nc))))))

;; inverter's delay : id
;; or-gate's delay : od
;; and-gate-delay : ad

;; one HA's delay = max(2ad + id, od + ad) : had
;; one FA's delay = 2had + od : fad

;; n-bit ripple-carry adder's delay = n*fad
;; = n(2had + od)
;; = 2n*had + n*od

;; if ad + id > od
;; = 2n(2ad + id) + n*od
;; = 4n*ad + 2n*id + n*od

;; if ad + id < od
;; = 2n*(od + ad) + n*od
;; = 3n*od + 2n*ad

		
;; Representing wires


(define (make-wire)
  (let ((signal-value 0)
	(action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures)
		 'done)))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operration -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
	((car procedures))
	(call-each (cdr procedures)))))


(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))



;; The agenda

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

;; Implementing the agenda

;; segment => (time . queue)
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s)
  (car s))

(define (segment-queue s)
  (cdr s))

;; agenda => (time . segments)
(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments))
		       action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr!
	       segments
	       (cons (make-new-time-segment time action)
		     (cdr segments)))
	      (add-to-segments! rest)))))
    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
	  (set-segments!
	   agenda
	   (cons (make-new-time-segment time action)
		 segments))
	   (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))



(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))

;; A sample simulation

(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (newline)
		 (display name)
		 (display " ")
		 (display (current-time the-agenda))
		 (display " New-value = ")
		 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)

(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)

(set-signal! input-2 1)

(propagate)

;; ex 3.31

(define (make-wire-without-init)
  (let ((signal-value 0)
	(action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures)
		 'done)))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
;;      (proc))
      )

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operration -- WIRE" m))))
    dispatch))



(define the-agenda (make-agenda))
(define input-1 (make-wire-without-init))
(define input-2 (make-wire-without-init))
(define sum (make-wire-without-init))
(define carry (make-wire-without-init))

(probe 'sum sum)

(probe 'carry carry)

(half-adder input-1 input-2 sum carry) 

(set-signal! input-1 1)

(propagate)
;; none...
(set-signal! input-2 1)

(propagate)
;; loose result
;; gates aren't run proc immediately. they just asign event handler to wire.


;; ex 3.32

;; segment => (time . queue)
;; segments => (segment+)
;; agenda => (time . segments)

(define the-agenda (make-agenda))
(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))

(set-signal! input-2 1)

(and-gate input-1 input-2 output)

(set-signal! input-1 1)
;;  [input-1's event[0->1]]

(set-signal! input-2 0)
;; [input-1's event[0->1], input-2's event[1->0]]

(propagate)

;; if segment has queue..
;; fire input-1 0->1,  then output => (1 and 1) = 1
;; fire input-2 1->0, then output => (0 and 1) = 0

;; if segment has stack.
;; fire input-2 1->0, then output => (0 and 0) = 0
;; fire input-1 0->1,  then output => (0 and 1) = 0

