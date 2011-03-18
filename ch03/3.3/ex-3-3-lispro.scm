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


;3.28
(define (or-gate a1 a2 output)
(define (or-action-procedure)
(let ((new-value
(logical-or (get-signal a1) (get-signal a2))))
(after-delay or-gate-delay
(lambda ()
(set-signal! output new-value)))))
(add-action! a1 or-action-procedure)
(add-action! a2 or-action-procedure)
‘ok)
(define (logical-or x y)
(if (and (= x 0) (= y 0))
0
1))

;3.29
(define (or-gate a1 a2 output)
(let ((c (make-wire)) (d (make-wire)) (e (make-wire)))
(inverter a1 c)
(inverter a2 d)
(and-gate c d e)
(inverter e output))
‘ok)
;3.30
(define (half-adder a b s c)
(let ((d (make-wire)) (e (make-wire)))
(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)
‘ok))
(define (full-adder a b c-in sum c-out)
(let ((s (make-wire))
(c1 (make-wire))
(c2 (make-wire)))
(half-adder b c-in s c1)
(half-adder a s sum c2)
(or-gate c1 c2 c-out)
‘ok))
(define (inverter input output)
(define (invert-input)
(let ((new-value (logical-not (get-signal input))))
(after-delay inverter-delay
(lambda ()
(set-signal! output new-value)))))
(add-action! input invert-input)
‘ok)
(define (logical-not s)
(cond ((= s 0) 1)
((= s 1) 0)
(else (error “Invalid signal” s))))
(define (and-gate a1 a2 output)
(define (and-action-procedure)
(let ((new-value
(logical-and (get-signal a1) (get-signal a2))))
(after-delay and-gate-delay
(lambda ()
(set-signal! output new-value)))))
(add-action! a1 and-action-procedure)
(add-action! a2 and-action-procedure)
‘ok)
(define (logical-and x y)
(if (and (= x 1) (= y 1))
1
0))

(define (or-gate a1 a2 output)
(define (or-action-procedure)
(let ((new-value
(logical-or (get-signal a1) (get-signal a2))))
(after-delay or-gate-delay
(lambda ()
(set-signal! output new-value)))))
(add-action! a1 or-action-procedure)
(add-action! a2 or-action-procedure)
‘ok)
(define (logical-or x y)
(if (and (= x 0) (= y 0))
0
1))

; answer
(define (ripple-carry-adder An Bn Sn C)
(define (r-c-adder-iter Ak Bk Sk Ck)
(cond ((null? Ak) null) ;
(else
(full-adder (car Ak) (car Bk) Ck (car Sk) Ck)
(r-c-adder-iter (cdr Ak) (cdr Bk) (cdr Sk) Ck))))
(r-c-adder-iter An Bn Sn C))
;3.31
(define (half-adder a b s c)
(let ((d (make-wire)) (e (make-wire)))
(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)
‘ok))
(define (full-adder a b c-in sum c-out)
(let ((s (make-wire))
(c1 (make-wire))
(c2 (make-wire)))
(half-adder b c-in s c1)
(half-adder a s sum c2)
(or-gate c1 c2 c-out)
‘ok))
(define (inverter input output)
(define (invert-input)
(let ((new-value (logical-not (get-signal input))))
(after-delay inverter-delay
(lambda ()
(set-signal! output new-value)))))
(add-action! input invert-input)
‘ok)
(define (logical-not s)
(cond ((= s 0) 1)
((= s 1) 0)
(else (error “Invalid signal” s))))
(define (and-gate a1 a2 output)
(define (and-action-procedure)
(let ((new-value
(logical-and (get-signal a1) (get-signal a2))))
(after-delay and-gate-delay
(lambda ()
(set-signal! output new-value)))))
(add-action! a1 and-action-procedure)
(add-action! a2 and-action-procedure)
‘ok)
(define (logical-and x y)
(if (and (= x 1) (= y 1))
1
0))

(define (or-gate a1 a2 output)
(define (or-action-procedure)
(let ((new-value
(logical-or (get-signal a1) (get-signal a2))))
(after-delay or-gate-delay
(lambda ()
(set-signal! output new-value)))))
(add-action! a1 or-action-procedure)
(add-action! a2 or-action-procedure)
‘ok)
(define (logical-or x y)
(if (and (= x 0) (= y 0))
0
1))

(define (call-each procedures)
(if (null? procedures)
‘done
(begin
((car procedures))
(call-each (cdr procedures)))))

(define (get-signal wire)
(wire ‘get-signal))
(define (set-signal! wire new-value)
((wire ‘set-signal!) new-value))
(define (add-action! wire action-procedure)
((wire ‘add-action!) action-procedure))
(define (after-delay delay action)
(add-to-agenda! (+ delay (current-time the-agenda))
action
the-agenda))

(define (propagate)
(if (empty-agenda? the-agenda)
‘done
(let ((first-item (first-agenda-item the-agenda)))
(first-item)
(remove-first-agenda-item! the-agenda)
(propagate))))
(define (probe name wire)
(add-action! wire
(lambda ()        
(newline)
(display name)
(display “ “)
(display (current-time the-agenda))
(display “  New-value = “)
(display (get-signal wire)))))
(define (make-time-segment time queue)
(cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))
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
(set-segments!  agenda (cons (make-new-time-segment time action)
segments))
(add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
(let ((q (segment-queue (first-segment agenda))))
(delete-queue! q)
(if (empty-queue? q)
(set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
(if (empty-agenda? agenda)
(error “Agenda is empty—FIRST-AGENDA-ITEM”)
(let ((first-seg (first-segment agenda)))
(set-current-time! agenda (segment-time first-seg))
(front-queue (segment-queue first-seg)))))
(define the-agenda (make-agenda))
; SECTION 3.3.2
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons ‘() ‘()))
(define (front-queue queue)
(if (empty-queue? queue)
(error “FRONT called with an empty queue” queue)
(car (front-ptr queue))))
(define (insert-queue! queue item)
(let ((new-pair (cons item ‘())))
(cond ((empty-queue? queue)
(set-front-ptr! queue new-pair) (set-rear-ptr! queue new-pair) queue)
(else
(set-cdr! (rear-ptr queue) new-pair) (set-rear-ptr! queue new-pair) queue))))
(define (delete-queue! queue)
(cond ((empty-queue? queue)
(error “DELETE! called with an empty queue” queue))
(else
(set-front-ptr! queue (cdr (front-ptr queue))) queue)))
; make-wire
(define (make-wire)
(let ((signal-value 0) (action-procedures ‘()))
(define (set-my-signal! new-value)
(if (not (= signal-value new-value))
(begin (set! signal-value new-value)
(call-each action-procedures))
‘done))
(define (accept-action-procedure! proc)
(set! action-procedures (cons proc action-procedures))
;(proc))     
      )
(define (dispatch m)
(cond ((eq? m ‘get-signal) signal-value)
((eq? m ‘set-signal!) set-my-signal!)
((eq? m ‘add-action!) accept-action-procedure!)
(else (error “Unknown operation—WIRE” m)))) dispatch))
; execute
(define a (make-wire))
(define b (make-wire))
(define s (make-wire))
(define c (make-wire))
(define and-gate-delay 5)
(define inverter-delay 5)
(define or-gate-delay 5)

(half-adder a b s c)
(probe ‘input-a a) (probe ‘input-b b) (probe ‘sum s) (probe ‘carry c)
(newline)
(set-signal! a 1)
(propagate)
;3.32
;;; 밝히라니... 허거덕...

;3.33
(define (averager a b c)
(define (process-new-value)
(cond ((and (has-value? a) (has-value? b))
(set-value! c
(/ (+ (get-value a) (get-value b)) 2) me))
((and (has-value? a) (has-value? c))
(set-value! b
(- (* (get-value c) 2) (get-value a)) me))
((and (has-value? b) (has-value? c))
(set-value! a
(- (* (get-value c) 2) (get-value b)) me))))
(define (process-forget-value)
(forget-value! c me)
(forget-value! a me)
(forget-value! b me)
(process-new-value))
(define (me request)
(cond ((eq? request ‘I-have-a-value)  
(process-new-value))
((eq? request ‘I-lost-my-value) 
(process-forget-value))
(else 
(error “Unknown request—AVERAGER” request))))
(connect a me)
(connect b me)
(connect c me) me)
; execute
(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(probe “Input A” A)
(probe “Input B” B)
(probe “AVERAGE” C)

(averager A B C)
(set-value! A 25 ‘user)
(set-value! B 55 ‘user)
(forget-value! A ‘user)

(set-value! C 80 ‘user)
;3.34
(define true (= 0 0))
(define false (= 0 1))

;;;SECTION 3.3.5
(define (adder a1 a2 sum)
(define (process-new-value)
(cond ((and (has-value? a1) (has-value? a2))
(set-value! sum
(+ (get-value a1) (get-value a2)) me))
((and (has-value? a1) (has-value? sum))
(set-value! a2
(- (get-value sum) (get-value a1)) me))
((and (has-value? a2) (has-value? sum))
(set-value! a1
(- (get-value sum) (get-value a2)) me))))
(define (process-forget-value)
(forget-value! sum me)
(forget-value! a1 me)
(forget-value! a2 me)
(process-new-value))
(define (me request)
(cond ((eq? request ‘I-have-a-value)  
(process-new-value))
((eq? request ‘I-lost-my-value) 
(process-forget-value))
(else 
(error “Unknown request—ADDER” request))))
(connect a1 me)
(connect a2 me)
(connect sum me) me)
(define (inform-about-value constraint)
(constraint ‘I-have-a-value))
(define (inform-about-no-value constraint)
(constraint ‘I-lost-my-value))
(define (multiplier m1 m2 product)
(define (process-new-value)
(cond ((or (and (has-value? m1) (= (get-value m1) 0))
(and (has-value? m2) (= (get-value m2) 0)))
(set-value! product 0 me))
((and (has-value? m1) (has-value? m2))
(set-value! product
(* (get-value m1) (get-value m2)) me))
((and (has-value? product) (has-value? m1))
(set-value! m2
(/ (get-value product) (get-value m1)) me))
((and (has-value? product) (has-value? m2))
(set-value! m1
(/ (get-value product) (get-value m2)) me))))
(define (process-forget-value)
(forget-value! product me)
(forget-value! m1 me)
(forget-value! m2 me)
(process-new-value))
(define (me request)
(cond ((eq? request ‘I-have-a-value)
(process-new-value))
((eq? request ‘I-lost-my-value)
(process-forget-value))
(else
(error “Unknown request—MULTIPLIER” request))))
(connect m1 me)
(connect m2 me)
(connect product me) me)
(define (constant value connector)
(define (me request)
(error “Unknown request—CONSTANT” request))
(connect connector me)
(set-value! connector value me) me)
(define (probe name connector)
(define (print-probe value)
(newline)
(display “Probe: “)
(display name)
(display “ = “)
(display value))
(define (process-new-value)
(print-probe (get-value connector)))
(define (process-forget-value)
(print-probe “?”))
(define (me request)
(cond ((eq? request ‘I-have-a-value)
(process-new-value))
((eq? request ‘I-lost-my-value)
(process-forget-value))
(else
(error “Unknown request—PROBE” request)))) (connect connector me) me)
(define (make-connector)
(let ((value false) (informant false) (constraints ‘()))
(define (set-my-value newval setter)
(cond ((not (has-value? me))
(set! value newval)
(set! informant setter)
(for-each-except setter
inform-about-value constraints))
((not (= value newval))
(error “Contradiction” (list value newval)))
(else ‘ignored)))
(define (forget-my-value retractor)
(if (eq? retractor informant)
(begin (set! informant false)
(for-each-except retractor
inform-about-no-value constraints))
‘ignored))
(define (connect new-constraint)
(if (not (memq new-constraint constraints))
(set! constraints 
(cons new-constraint constraints)))
(if (has-value? me)
(inform-about-value new-constraint))
‘done)
(define (me request)
(cond ((eq? request ‘has-value?)
(if informant true false))
((eq? request ‘value) value)
((eq? request ‘set-value!) set-my-value)
((eq? request ‘forget) forget-my-value)
((eq? request ‘connect) connect)
(else (error “Unknown operation—CONNECTOR”
request)))) me))
(define (for-each-except exception procedure list)
(define (loop items)
(cond ((null? items) ‘done)
((eq? (car items) exception) (loop (cdr items)))
(else (procedure (car items))
(loop (cdr items)))))
(loop list))
(define (has-value? connector)
(connector ‘has-value?))
(define (get-value connector)
(connector ‘value))
(define (set-value! connector new-value informant)
((connector ‘set-value!) new-value informant))
(define (forget-value! connector retractor)
((connector ‘forget) retractor))
(define (connect connector new-constraint)
((connector ‘connect) new-constraint))
; exercise 3.34
(define (squarer a b)
(multiplier a a b))
; execute
(define A (make-connector))
(define B (make-connector))

(probe “Input A” A)
(probe “Output B” B)

(squarer A B)
(set-value! A 5 ‘user)
(forget-value! A ‘user)
(set-value! B 16 ‘user)
;;squarer를 구할 때 (multiplier a a b)를 사용하기에

;;a를 지우고b에만 값을 설정하고a를 구하려고 하면m1, m2모두 지워집니다.
;;따라서 이것으로는 구할 수 없습니다.
;3.35
(define (square x) (* x x))

(define (squarer a b)
(define (process-new-value)
(if (has-value? b)
(if (< (get-value b) 0)
(error “square less than 0 -- SQUARER” (get-value b))
(set-value! a
(sqrt (get-value b)) me))
(set-value! b
(square (get-value a)) me)))
(define (process-forget-value)
(forget-value! b me)
(forget-value! a me))
(define (me request)
(cond ((eq? request ‘I-have-a-value)
(process-new-value))
((eq? request ‘I-lost-my-value)
(process-forget-value))
(else
(error “Unknown request—SQUARER” request))))
(connect a me)
(connect b me) me)
; execute
(define A (make-connector))
(define B (make-connector))

(probe “Input A” A)
(probe “Output B” B)

(squarer A B)
(set-value! A 5 ‘user)
(forget-value! A ‘user)
(set-value! B 16 ‘user)

;3.36
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;          +----------------------------------------------------------------------------------------------+
;  global  | b -------------------------------------------------------------------------+                 |
;  env     | for-each-except-------------------------+                                  |                 |
;          | a ----+                                 |                                  |                 |
;          +-------+-----------^---------------------+----------------------------------+------------^----+
;                  |           |                     |                                  |            |
;                  |  +------------------+    +------+---------+                        |   +-------------------+
;                  |  |    value : 10    |    |      | +----------------+               |   |   value : false   |
;          +-------+  | informant : user |    |      | |    loop :      |<-+            |   | informant : false |
;          |          |constranints:null |    |      V +----------+-----+  |            |   | constraints : null|
;          |          |set-my-value :    +-+  |    ㅇㅇ--------^  |        |            |   +-------------------+
;          |          |forget-my-value : | |  |     |             |        |          ㅇㅇ------------^
;          V          |     connect :    | |  |     +-------+     +----+   |           |
;        ㅇㅇ---+     |       me :       | |  |             V          |   |           V
;         |     |     +--------^---------+ V  | parameters:exception,  |   | parameters : f
;         +-+   +--------------+          ㅇㅇ+--+         procedure,  |   | body : (let ((table (make-table)))
;           V                  |-----------+--+  |         list        |   |    (lambda (x)
;  parameters :                +-----------+-----+    body:(loop list) |   |      (let ((previously-computed-result (lookup x table)))
;  body :                                  +---------------+           V   |        (or previously-computed-result
;  (let ((value false) (informant false) constraints '())) |          ㅇㅇ-+           (let ((result (f x)))
;      me)                                                 V           |                 (insert! x result table)
;                                    parameters : newval, setter       +-----V            result)))))
;                                    body :                                  parameters : items
;                                          (cond ((not (has-value me))       body :
;                                    (set! value newval)                     (cond ((null? items) 'done)
;                                    (set! informant setter)                       ((eq? (car items) exception) (loop (cdr
;                                    (for-eace-except setter                 items)))
;                                              inform-about-value                  (else (procedure (car items)))
;                                              constraints))                            (loop (car items))))
;                                    ((not (=value newval))
;                                     (error "Contradiction" (list value newval)))
;                                    (else "ignored))

;3.37
(define (c+ x y)
(let ((z (make-connector)))
(adder x y z) z))
; answer
(define (c- x y) ; x - y
(let ((z (make-connector)))
(adder z y x) z))
(define (c* x y) ; x * y
(let ((z (make-connector)))
(multiplier x y z) z))
(define (c/ x y) ; x / y
(let ((z (make-connector)))
(multiplier z y x) z))
(define (cv value)
(let ((z (make-connector)))
(set-value! z value ‘user) z))
; exercise 3.37
(define (celsius-fahrenheit-converter x)
(c+ (c* (c/ (cv 9) (cv 5))
x)
(cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
; execute
(probe “Celsius temp” C)
(probe “Fahrenheit temp” F)

(set-value! C 25 ‘user)
(forget-value! C ‘user)
(set-value! F 212 ‘user)
