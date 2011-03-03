;;;;;;ex3.12
(define (append x y)
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
(define y (list 'c 'd))
(define z (append  x y))
z
(cdr x)

(define w (append! x y))
w
(cdr x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   +---+---+   +---+---+
;x->| ● | ●-+-->| ● | ●-+-+
;   +-+-+---+   +-+-+---+ |
;     |           |       |
;     V           V       |
;   +---+       +---+     |
;   | a |       | b |     |
;   +---+       +---+     |
;   +---------------------+
;   |  +---+---+   +---+---+
;y->+->| ● | ●-+-->| ● | / |
;      +-+-+---+   +-+-+---+
;        |           |
;        V           V
;      +---+       +---+
;      | c |       | d |
;      +---+       +---+




; ex3.13
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

; execute
z
(last-pair z)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; +------------------------------------+
; | +---+---+   +---+---+   +---+---+  |
;z+>| ● | ●-+-->| ● | ●-+-->| ● | ●-+--+
;   +-+-+---+   +-+-+---+   +-+-+---+  
;     |           |           |
;     V           V           V
;   +---+       +---+       +---+
;   | a |       | b |       | c |
;   +---+       +---+       +---+




; ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; execute
(define v (list 'a 'b 'c 'd))
v
(newline)
(define w (mystery v))
v
w


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;   +---+---+   +---+---+   +---+---+  +---+---+
;v->| ● | ●-+-->| ● | ●-+-->| ● | ●-+--| ● | / |
;   +-+-+---+   +-+-+---+   +-+-+---+  +-+-+---+
;     |           |           |          |
;     V           V           V          V
;   +---+       +---+       +---+      +---+
;   | a |       | b |       | c |      | d |
;   +---+       +---+       +---+      +---+

;   +---+---+   +---+---+   +---+---+  +---+---+
;w->| ● | ●-+-->| ● | ●-+-->| ● | ●-+--| ● | / |
;   +-+-+---+   +-+-+---+   +-+-+---+  +-+-+---+
;     |           |           |          |
;     V           V           V          V
;   +---+       +---+       +---+      +---+
;   | d |       | c |       | b |      | a |
;   +---+       +---+       +---+      +---+

;ex 3.15
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    +---+---+
;z1->| ● | ● |
;    +-+-+-+-+
;      |   |   
;      V   V   
;    +---+---+   +---+---+
;x-->| ● | ●-+-->| ● | / |
;    +-+-+---+   +-+-+---+
;      |           |
;      V           V
;    +---+       +---+
;    |wow|       | b |
;    +---+       +---+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    +---+---+   +---+---+   +---+---+
;z2->| ● | ●-+-->| ● | ●-+-->| ● | / |
;    +-+-+---+   +-+-+---+   +-+-+---+
;      |           |           |
;      |           V           V
;      |         +---+       +---+
;      |         | a |       | b |
;      |         +---+       +---+
;      |                       ^
;      |                       |
;      |         +---+---+   +-+-+---+
;      +-------->| ● | ●-+-->| ● | / |
;                +-+-+---+   +---+---+
;                  |
;                  V
;                +---+
;                |wow|
;                +---+

; ex 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
; 처음 구상한 코드
;; answer
;(define b (list 2))
;b
;(newline)
;; 3
;(list 1 2 3)
;(count-pairs (list 1 2 3))
;(newline)
;; 4
;(define c (cons 1 b)) (define d (cons c b))
;d
;(count-pairs d)
;(newline)
;; 7
;(define e (cons b b)) (define f (cons e e))
;f
;(count-pairs f)
;(newline)
;; infinite
;(set-cdr! b f)
;(count-pairs f)

; 보기 쉽게 새롭게 구성
; answer
(define a (cons 1 2))
(define b (cons 3 4))
(define c (cons 5 6))
a b c
(newline)
; 3
(set-cdr! a b) (set-cdr! b c) (set-cdr! c null)
a (count-pairs a)
(newline)
; 4
(set-car! a b) (set-cdr! a c) (set-cdr! b c)
a (count-pairs a)
(newline)
; 7
(set-car! a b) (set-cdr! a b) (set-car! b c) (set-cdr! b c)
a (count-pairs a)
(newline)
; infinite
(set-cdr! c a)
(count-pairs a)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    +---+---+
; a->| ● | ● |
;    +-+-+-+-+
;      |   |   
;      V   V   
;    +---+---+
;    | 1 | 2 |
;    +---+---+

;    +---+---+
; b->| ● | ● |
;    +-+-+-+-+
;      |   |   
;      V   V   
;    +---+---+
;    | 3 | 4 |
;    +---+---+

;    +---+---+
; c->| ● | ● |
;    +-+-+-+-+
;      |   |   
;      V   V   
;    +---+---+
;    | 5 | 6 |
;    +---+---+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;              b---+       c--+
                   V          V
;    +---+---+   +---+---+  +---+---+
; a->| ● | ●-+-->| ● | ●-+->| ● | / |
;    +-+-+---+   +-+-+---+  +-+-+---+
;      |           |          |
;      V           V          V
;    +---+       +---+      +---+
;    | 1 |       | 3 |      | 5 |
;    +---+       +---+      +---+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;              c---+      
                   V      
;    +---+---+   +---+---+
; a->| ● | ●-+-->| ● | / |
;    +-+-+---+   +-+-+---+
;      |           |
;      V           V
;    +---+---+   +---+
; b->| ● | ●-+-->| 5 |
;    +---+---+   +---+
;      |  
;      V  
;    +---+
;    | 3 |
;    +---+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;             
              
;    +---+---+
; a->| ● | ● |
;    +-+-+-+-+
;      +---|   c---+
;      V           V
;    +---+---+   +---+---+
; b->| ● | ●-+-+>| ● | / |
;    +---+---+ | +-+-+---+
;      |       |   |
;      +-------+   V
;                +---+
;                | 5 |
;                +---+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      +---------------+      
       V               |
;    +---+---+         |
; a->| ● | ● |         |
;    +-+-+-+-+         |
;      +---|   c---+   |
;      V           V   |
;    +---+---+   +---+-+-+
; b->| ● | ●-+-+>| ● | ● |
;    +---+---+ | +-+-+---+
;      |       |   |
;      +-------+   V
;                +---+
;                | 5 |
;                +---+

;ex 3.17
 (define (count-pairs x) 
   (let ((encountered '())) 
     (define (helper x) 
       (if (or (not (pair? x)) (memq x encountered)) 
         0 
         (begin 
           (set! encountered (cons x encountered)) 
           (+ (helper (car x)) 
              (helper (cdr x)) 
              1)))) 
   (helper x)))

;ex 3.18

;ex 3.19
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
  
  
 ; Tested with mzscheme implementation of R5RS: 
 (define x '(1 2 3 4 5 6 7 8)) 
 (define y '(1 2 3 4 5 6 7 8)) 
 (set-cdr! (cdddr (cddddr y)) (cdddr y)) 
 (define z '(1)) 
 (set-cdr! z z) 
 x ; (1 2 3 4 5 6 7 8) 
 y ; (1 2 3 . #0=(4 5 6 7 8 . #0#)) 
 z ; #0=(1 . #0#) 
 (contains-cycle? x) ; #f 
 (contains-cycle? y) ; #t 
 (contains-cycle? z) ; #t 

;;;;;;ex 3.20
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)

(car x)

;             
;             (define x (cons 1 2))
; global   +------------------------+
; env      | x    -+                |
;          |       |                |
;           -------+-----------^----+ 
;                  |           |
;                  |        +-----+
;                  |   E1-->|car:1|
;                  |        |cdr:2|
;                  V        +-----+
;             +---+---+        ^
;             | ● | ● |--------+
;             +-+-+---+
;               |
;               V
;        Parameters :
;        Body :
;        (cons 1 2)
;             





;             (define z (cons x x))
; global   +--------------------------------+
; env      | z    -----------------------+  |
;          | x    -+                     |  |
;           -------+-----------^---------+--+
;                  |           |         |
;                  |        +------+     |     +-----+
;                  |   E1-->|car:1 |<----+-----|car:x|<--E2
;                  |        |cdr:2 |     |     |cdr:x|
;                  V        +------+     V     +-----+
;             +---+---+        ^    +---+---+     ^  
;             | ● | ● |--------+    | ● | ● |-----+
;             +-+-+---+             +-+-+---+
;               |                     |
;               V                     V
;        Parameters :           Parameters :
;        Body :                 Body :
;        (cons 1 2)             (cons x x)





;             (set-car! (cdr z) 17)
; global   +--------------------------------+
; env      | z    -----------------------+  |
;          | x    -+                     |  |
;           -------+-----------^---------+--+
;                  |           |         |
;                  |        +------+     |     +-----+
;                  |   E1-->|car:17|<----+-----|car:x|<--E2
;                  |        |cdr:2 |     |     |cdr:x|
;                  V        +------+     V     +-----+
;             +---+---+        ^    +---+---+     ^  
;             | ● | ● |--------+    | ● | ● |-----+
;             +-+-+---+             +-+-+---+
;               |                     |
;               V                     V
;        Parameters :           Parameters :
;        Body :                 Body :
;        (cons 1 2)             (cons x x)

;;; ex. 3.21

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

;; EXERCISE 3.21
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)
(newline)

; answer
(define (print-queue queue)
  (define (print-iter q)
    (cond ((null? q) null)
          (else (display (car q))
                (display " ")
                (print-iter (cdr q)))))
  (print-iter (front-ptr queue))
  (newline))
; execute
(define q1 (make-queue))
(print-queue q1)
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

;;; ex 3.22
(define (make-queue)
  (let ((front-ptr null) ; 처음에는 빈 큐이므로 null
        (rear-ptr null)) ; 처음에는 빈 큐이므로 null
    ; insert-queue 정의
    (define (insert-queue item)
      (let ((new-pair (cons item '())))
        (cond ((null? front-ptr)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    ; delete-queue 정의
    (define (delete-queue)
      (cond ((null? front-ptr)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr)))))
    ; print-queue 정의
    (define (print-queue)
      (define (print-iter q)
        (cond ((null? q) null)
              (else (display (car q))
                    (display " ")
                    (print-iter (cdr q)))))
      (print-iter front-ptr)
      (newline))
    ; dispatch 정의
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?)
             (null? front-ptr))
            ((eq? m 'front-queue?)
             (if (null? front-ptr)
                 (error "FRONT called with an empty queue")
                 (car front-ptr)))
            ((eq? m 'insert-queue!) insert-queue)
            ((eq? m 'delete-queue!) (delete-queue))
            ((eq? m 'print-queue) (print-queue))))
    dispatch))

; execute
(define q1 (make-queue))
(q1 'print-queue)
((q1 'insert-queue!) 'a)
(q1 'print-queue)
((q1 'insert-queue!) 'b)
(q1 'print-queue)
(q1 'delete-queue!)
(q1 'print-queue)
(q1 'delete-queue!)
(q1 'print-queue)

;;; ex 3.23
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))
(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-cdr! (cdr new-pair) (front-ptr deque))
           (set-car! (cdr (front-ptr deque)) new-pair)
           (set-front-ptr! deque new-pair)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-cdr! (cdr (rear-ptr deque)) new-pair)
           (set-car! (cdr new-pair) (rear-ptr deque))
           (set-rear-ptr! deque new-pair)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (cdr (cdr (front-ptr deque))))
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-rear-ptr! deque (car (cdr (rear-ptr deque))))
         (set-cdr! (cdr (rear-ptr deque)) '()) ; rear가 된 것에 cdr을 null로 맞춤.
         deque)))


(define (print-deque deque)
  (define (print-iter q)
    (cond ((null? q) null)
          (else (display (car q))
                (display " ")
                (print-iter (cdr (cdr q))))))
  (print-iter (front-ptr deque))
  (newline))

; execute
(define q1 (make-deque))
(print-deque q1) ; null
(front-insert-deque! q1 'a)
(print-deque q1) ; a
(rear-insert-deque! q1 'b)
(print-deque q1) ; a b
(front-insert-deque! q1 'c)
(print-deque q1) ; c a b
(rear-insert-deque! q1 'd)
(print-deque q1) ; c a b d
(rear-delete-deque! q1)
(print-deque q1) ; c a b
(front-delete-deque! q1)
(print-deque q1) ; a b
(rear-delete-deque! q1)
(print-deque q1) ; a
(front-delete-deque! q1)
(print-deque q1) ; null

;; ex 3.24
(define false (= 0 1))
;;;SECTION 3.3.3
;; local tables
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable) same-key?)))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable) same-key?)))
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

(define (assoc key records same-key?)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records) same-key?))))

; execute
(define t1 (make-table equal?))
((t1 'insert-proc!) 'letters 'a 97)
((t1 'insert-proc!) 'letters 'b 98)
((t1 'insert-proc!) 'math '+ 43)
((t1 'insert-proc!) 'math '- 45)
((t1 'insert-proc!) 'math '* 42)
(newline)
((t1 'lookup-proc) 'math '*)
((t1 'lookup-proc) 'letters 'a)
((t1 'lookup-proc) 'math 'b)
((t1 'lookup-proc) 'letterss 'b)
(newline) (newline)

; same-key?를 다른 것으로 정의한다.
(define (consider_tol a b)
  (< (abs (- b a)) 2))

(define t2 (make-table consider_tol))
((t2 'insert-proc!) 10 100 97)
((t2 'insert-proc!) 10 101 98)
((t2 'insert-proc!) 20 50 43)
((t2 'insert-proc!) 20 51 45)
((t2 'insert-proc!) 11 55 42)
((t2 'insert-proc!) 21 55 42)
(newline)
((t2 'lookup-proc) 10 100)
((t2 'lookup-proc) 10 101)
((t2 'lookup-proc) 20 50)
((t2 'lookup-proc) 20 51)
((t2 'lookup-proc) 20 55)
((t2 'lookup-proc) 15 55)

;;;ex 3.25

(define false (= 0 1))
;;;SECTION 3.3.3
;; local tables
; answer
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup . key-list)
      (define (lookup-iter keys subtable)
        (let ((record (assoc (car keys) (cdr subtable) same-key?)))
          (if record ; key에 맞는 것이 있는지 확인
              (if (null? (cdr keys)) ; (car key) 값이 마지막인지 확인
                  (cdr record)
                  (lookup-iter (cdr keys) record))
              false)))
      (lookup-iter key-list local-table))
    (define (insert! value . key-list)
      (define (insert-lookup-iter keys subtable)
        (let ((record (assoc (car keys) (cdr subtable) same-key?)))
          (if record ; key에 맞는 것이 있는지 확인
              (if (null? (cdr keys)) ; (car key) 값이 마지막인지 확인
                  (set-cdr! record value)
                  (insert-lookup-iter (cdr keys) record))  
              (cond ((null? (cdr keys)) ; (car key) 값이 마지막인지 확인
                     (set-cdr! subtable
                               (cons (cons (car keys) value)
                                     (cdr subtable)))) ; key에 맞는 subtable 작성
                    (else
                     (set-cdr! subtable
                               (cons (list (car keys)
                                           (make-subtable (cdr keys)))
                                     (cdr subtable))))))))
      (define (make-subtable sub-keys)
        (if (null? (cdr sub-keys)) ; key 리스트 마지막?
            (cons (car sub-keys) value)
            (cons (car sub-keys) (make-subtable (cdr sub-keys)))))
      (insert-lookup-iter key-list local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (assoc key records same-key?)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records) same-key?))))

; execute
(define t1 (make-table equal?))
((t1 'insert-proc!) 97 'letters 'a)
((t1 'insert-proc!) 98 'letters 'b)
((t1 'insert-proc!) 43 'math '+)
((t1 'insert-proc!) 45 'math '-)
((t1 'insert-proc!) 42 'math '*)
(newline)
((t1 'lookup-proc) 'math '*)
((t1 'lookup-proc) 'letters 'a)
((t1 'lookup-proc) 'math 'b)
((t1 'lookup-proc) 'letterss 'b)
(newline) (newline)

; same-key?를 다른 것으로 정의한다.
(define (consider_tol a b)
  (< (abs (- b a)) 2))

(define t2 (make-table consider_tol))
((t2 'insert-proc!) 97 10 100)
((t2 'insert-proc!) 98 10 101)
((t2 'insert-proc!) 43 20 50)
((t2 'insert-proc!) 45 20 51)
((t2 'insert-proc!) 42 11 55)
((t2 'insert-proc!) 42 21 55)
(newline)
((t2 'lookup-proc) 10 100)
((t2 'lookup-proc) 10 101)
((t2 'lookup-proc) 20 50)
((t2 'lookup-proc) 20 51)
((t2 'lookup-proc) 20 55)
((t2 'lookup-proc) 15 55)
(newline) (newline)

; execute exercise 3.25
(define t3 (make-table equal?))
((t3 'insert-proc!) 97 'letters 'a)
((t3 'insert-proc!) 98 'letters 'b)
;((t3 'insert-proc!) 100 'letters 'b 'kk) 추가하는 것은 구현하지 않음
((t3 'insert-proc!) 100 'letters 'c 'kk)
((t3 'insert-proc!) 43 'math '+)
((t3 'insert-proc!) 45 'math '-)
((t3 'insert-proc!) 42 'math '* 'kkk)
(newline)
((t3 'lookup-proc) 'math '*)
((t3 'lookup-proc) 'letters 'c 'kk)
((t3 'lookup-proc) 'math 'b)
((t3 'lookup-proc) 'letterss 'b)

;;ex 3.26 (==ex2.66)
(define true (= 0 0))
(define false (= 0 1))
; BINARY TREES
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

; answer
(define (lookup given-key set-of-records)
  (let ((less? (lambda (a b) (< a b))) ; key표현에 따라 변할 수 있음.
        (more? (lambda (a b) (> a b)))) ; key표현에 따라 변할 수 있음.
    (cond ((null? set-of-records) false)
          ((equal? given-key (key (entry set-of-records)))
           (entry set-of-records))
          ((less? given-key (key (entry set-of-records)))
           (lookup given-key (left-branch set-of-records)))
          ((more? given-key (key (entry set-of-records)))
           (lookup given-key (right-branch set-of-records))))))

;;;;ex 3.27



;             
; global+-----------------------------------+
; env   |memoize-------------------------+  |
;       |memo-fibo-+                     |  |<----+
;       +----------+-----------^---------+--+     |
;                  |           |         |        |
;                  |           |         |     +--+--+
;                  |           |         |     |table|<--here
;                  |           |         |     +-----+
;                  V           |         V        ^
;             +---+---+        |    +---+---+     |  
;             | ● | ● |--------+    | ● | ● |-----+
;             +-+-+---+             +-+-+---+
;               |                     |
;               V                     V
;        Parameters :			Parameters :
;        Body :				Body : (let ((table (make-table)))
;        (memoize (lambda (n)			(lambda (x)
;             (cond ((= n 0) 0)			 (let ((previously-computed-result (lookup x table)))
;                  ((= n 1) 1)			  (or previously-computed-result
;                  (else (+ (memo-fib 1 (- n 1))   (let ((result (f x)))
;                        (memo-fib 1 (- n 2)))))))  (insert! x result table)
;                                                    result)))))

(define false (= 0 1))
;;;SECTION 3.3.3

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

; exercise 3.27
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib1
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib1 (- n 1))
                            (memo-fib1 (- n 2))))))))

(define memo-fib2
  (memoize fib))

; check time
(define (timed-fib-test n case)
  (if (= case 1)
      (start-fib-test1 n (current-milliseconds))
      (start-fib-test2 n (current-milliseconds))))

(define (start-fib-test1 n start-time)
  (let ((fib-n (memo-fib1 n)))
    (if fib-n
        (report-fib n (- (current-milliseconds) start-time) fib-n)
        (= 0 1)
        )))

(define (start-fib-test2 n start-time)
  (let ((fib-n (memo-fib2 n)))
    (if fib-n
        (report-fib n (- (current-milliseconds) start-time) fib-n)
        (= 0 1)
        )))

(define (report-fib n elapsed-time result)
  (newline)
  (display "n : ")
  (display n)
  (display ",  time : ")
  (display elapsed-time)
  (display ",  result : ")
  (display result))

; execute
(timed-fib-test 3 1)
(timed-fib-test 3 2)
(timed-fib-test 30 1)
(timed-fib-test 30 2)
