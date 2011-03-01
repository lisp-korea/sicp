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




; ex3.12
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

;ex 3.18

;ex 3.19
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

 ;ex 3.20
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)

(car x)

;ex 3.9-2
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
