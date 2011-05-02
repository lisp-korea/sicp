
;; racket -l r5rs/run


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 3 모듈, 물체, 상태
;;; Ch 3.3 변형 가능한 데이터로 프로그래밍하기



;;;==========================================
;;; 3.3.1 변형가능한 리스트

(define x '((a b) (c d)))

x

(define y '(e f))

y

(set-car! x y)

(set-cdr! x '(1 2))


;;;--------------------------< ex 3.12 >--------------------------
;;; p331

(define (append2 x y)
  (if (null? x)
      y
      (cons (car x) (append2 (cdr x) y))))

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
;; x의 복사본 뒤에 y의 복사본을 잇는다.
z

(cdr x)
;; -> (b)


(define w (append! x y))
;; 원래 x의 제일 끝이 원래 y를 가리키도록 바꾼다.
w

(cdr x)
;; -> (b c d)




;;;--------------------------< ex 3.13 >--------------------------
;;; p332

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

z

;(last-pair z)
;; 무한 루프


;;;--------------------------< ex 3.14 >--------------------------
;;; p333

(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))


(define x '(1 2 3 4))

x

(mystery x)
;; x list의 각 cons셀에서 cdr이 가리키는 방향을 반대로(reverse) 바꾼다.

x


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 같이 쓰기와 이름의 실체
;;; p333

(define x (list 'a 'b))
(define z1 (cons x x))

z1

(define z2 (cons (list 'a 'b) (list 'a 'b)))

z2 

;; z1과 z2는 같은 값을 갖고 있지만 list구조는 다르다.


(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)
;; ((wow b) wow b)

(set-to-wow! z2)
;; ((wow b) a b)


;; eq? 는 두 변수가 서로 같은 곳을 가리키는지를 따진다.

;; 함께 쓰기를 하면 쌍으로 표현할 수 있는 데이터 구조가 많아진다.
;; 허나, 함께 쓰기는 아주 위험하다.
;; set-car!, set-cdr! 연산은 참말 조심해서 써야 한다!!!



;;;--------------------------< ex 3.15 >--------------------------
;; z1, z2 그리기


;;;--------------------------< ex 3.16 >--------------------------

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons 5 6))

(set-cdr! x y)
(set-cdr! y z)
(set-cdr! z '())

x

(count-pairs x)

;; [+|+]->[+|+]->[+|/]
;;  |      |      |   
;;  1      3      5

;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons 5 6))

(set-cdr! x y)
(set-car! x z)
(set-cdr! y z)
(set-cdr! z '())

x

(count-pairs x)
;; 4

;; [+|+]->[+|+]->[+|/]
;;  |      |      |^
;;  |      3      5|
;;  +--------------+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons 5 6))

(set-car! x y)
(set-cdr! x y)
(set-car! y z)
(set-cdr! y z)

x

(count-pairs x)
;; 7

;; [+|+]
;;  | | 
;; [+|+]
;;  | | 
;; [+|+]
;;  | |
;;  5 6

;;;;;;;;;;;;;;;;;;;;;;;;;
(define x (cons 1 2))

(set-cdr! x x)

;;(count-pairs x)
;; 무한 루프


;;;--------------------------< ex 3.17 >--------------------------
;;; p337

;;; 데이터 구조가 모두 몇 쌍으로 구성되는지 제대로 헤아릴 수 있는 count-pairs


(define (contain? pairs item)
  (if (null? pairs)
      #f
      (if (eq? (car pairs)
	       item)
	  #t
	  (is-in (cdr pairs) item))))

(contain? '(1 2 3) 1)
(contain? '(1 2 3) 4)

;; (define (count-unique pairs item)
;;   (if (contain? pairs item)
;;       0
;;       1))

(define (new-count-pairs a)
  (let ((pairs '()))
    (define (new-count-pairs x)
      (cond ((not (pair? x)) 0)
	    ((contain? pairs x) 0)
	    (else
	     (begin
	       (set! pairs (cons x pairs))
	       (+ (new-count-pairs (car x))
		  (new-count-pairs (cdr x))
		   1)))))
    (new-count-pairs a)))



(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons 5 6))

(set-cdr! x y)
(set-car! x z)
(set-cdr! y z)
(set-cdr! z '())

x

(new-count-pairs x)

;;;--------------------------< ex 3.18 >--------------------------
;;; p337
;;; 리스트 속에 고리(cycle)가 들어있는지 살펴보는 프로시저

;;; V

;; 조건에서 트리에서의 사이클도 찾을 수 있어야 하나?

(define (contain-cycle? lst)
  (let ((visited '()))
    (define (traverse x)
      (cond ((not (pair? x)) #f)
	    ((contain? visited (car x)) #t)
	    ((contain? visited (cdr x)) #t)
	    (else
	     (begin
	       (set! visited (cons x visited))
	       (or (traverse (car x))
		   (traverse (cdr x)))))))
    (traverse lst)))

(define z (make-cycle (list 'a 'b 'c)))

(contain-cycle? z)
;; ok

(contain-cycle? '(1 2 3 4))
;; ok


(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons 5 6))

(set-cdr! x y)
(set-car! x z)
(set-cdr! y z)
(set-cdr! z '())

x

(contain-cycle? x)
;; 이건 #f가 나와야 되는데,,,


;;;--------------------------< ex 3.19 >--------------------------

;;; 딱 정해진 만큼의 공간만 쓰는 알고리즘으로 3.18 다시 풀기

;;;,,



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 변했다는 말은 그저 덮어썼다는 뜻이다.
;;; p338

;; ch2.1.3 쌍을 프로시저로 표현
(define (new-cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (new-car z) (z 'car))

(define (new-cdr z) (z 'cdr))


;; 변형 가능한 데이터에서 쌍을 프로시저로 표현
(define (new-cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  ((eq? m 'set-car!) set-x!)
	  ((eq? m 'set-cdr!) set-y!)
	  (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (new-car z) (z 'car))

(define (new-cdr z) (z 'cdr))


(define (new-set-car! z new-value)
  ((z 'set-car!) new-value)
  z)


(define (new-set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)


(define p (new-cons 1 2))

(new-car p)
(new-cdr p)

(new-set-car! p 3)
(new-car p)

(new-set-cdr! p 4)
(new-cdr p)



;;;--------------------------< ex 3.20 >--------------------------

;; 환경 그림으로 아래 식을 셈하는 과정 설명

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)

(car x)



(define x (new-cons 1 2))
(define z (new-cons x x))
(new-set-car! (new-cdr z) 17)

(new-car x)




;;;==========================================
;;; 3.3.2 큐
;;; p339

;;; set-car!와 set-cdr!로 쌍을 고쳐쓸 수 있으면 cons, car, cdr로는 만들지 못하던 여러 데이터 구조를 표현할 수 있다.
;;; - 큐, 표

;;; 큐란 뒤로 원소를 집어넣고 앞으로 꺼낼 수 있는 차례열이다.

;; (define q (make-queue))

;; (insert-queue! q 'a)
;; (insert-queue! q 'b)
;; (delete-queue! q)
;; (insert-queue! q 'c)
;; (insert-queue! q 'd)
;; (delete-queue! q)

;; 큐의 짜맞추개
; (make-queue)

;; 고르개
;(empty-queue? <q>)
;(front-queue? <q>)

;; 바꾸개
;(insert-queue! <q> <item>)
;(delete-queue! <q>)

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


;;;--------------------------< ex 3.21 >--------------------------
;;; p344

(define q1 (make-queue))

(insert-queue! q1 'a)

(insert-queue! q1 'b)

(insert-queue! q1 'c)

(delete-queue! q1)

(delete-queue! q1)

;;; print-queue

(define (print-queue q)
  (display (front-ptr q)))

(print-queue q1)


;;;--------------------------< ex 3.22 >--------------------------
;;; p345
;;; 큐를 상태가 있는 프로시저로 표현

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))

    (define (empty-queue?)
      (null? front-ptr))

    (define (insert-queue)
      (lambda (item)
	(let ((new-pair (cons item '())))
	  (cond ((empty-queue?)
		 (set! front-ptr new-pair)
		 (set! rear-ptr new-pair))
		(else
		 (set-cdr! rear-ptr new-pair)
		 (set! rear-ptr new-pair))))))

    (define (delete-queue)
      (lambda ()
	(cond ((empty-queue?)
	       (error "DELETE! called for empty queue"))
	      (else
	       (let ((front front-ptr))
		 (set! front-ptr (cdr front-ptr))
		 front)))))
	       
    (define (dispatch m)
      (cond ((eq? m 'front-queue) 
	     (if (empty-queue?)
		 '()
		 (car front-ptr)))
	    ((eq? m 'insert-queue!) (insert-queue))
	    ((eq? m 'delete-queue!) (delete-queue))
	    ((eq? m 'print-queue)
	     (display front-ptr))
	    (else
	     (error "Undefine operation -- QUEUE" m))))
    dispatch))

(define q1 (make-queue))

(q1 'front-queue)
((q1 'insert-queue!) 1)
(q1 'print-queue)
((q1 'insert-queue!) 2)
(q1 'print-queue)
((q1 'delete-queue!))
(q1 'print-queue)


;;;--------------------------< ex 3.23 >--------------------------

;; 데크(deque, double-ended queue)라는 데이터 구조는 앞뒤 양쪽으로 원소를 넣고 뺄 수 있는 차례열이다.
;; 데크를 구현하라
;; 짜맞추개 : make-deque
;; 술어 : empty-deque?
;; 고르개 : front-deque, rear-deque
;; 바꾸개 : front-insert-deque!, 
;;         rear-insert-deque!,
;;         front-delete-deque!,
;;         rear-delete-deque!

;; deque item :  <-|head|item|tail|-> 
;;
;;                (head item . tail)

(define (make-deque) (cons '() '()))

(define (front-deque-ptr deque) (car deque))

(define (rear-deque-ptr deque) (cdr deque))

;;;--------------
(define (set-front-deque! deque dqi) (set-car! deque dqi))
(define (set-rear-deque! deque dqi) (set-cdr! deque dqi))


;; (define (new-deque-item x) (cons '() (cons x '())))
;; (define (head-item item) (car item))
;; (define (tail-item item) (cddr item))
;; (define (val-item item) (cadr item))

;; <-()->
;;      +---+---+  +---+---+  
;; <----|   |   |->|   |   |  
;;      |   |   |  |   |   |--->
;;      +---+---+  +---+---+  
;;                   |
;;                   V
;;                 +---+
;;                 |   |
;;                 |   |
;;                 +---+

(define (new-deque-item x) (cons '() (cons x '())))
(define (head-ref-dqi dqi) dqi)
(define (tail-ref-dqi dqi) (cdr dqi))
(define (val-dqi dqi) (cadr dqi))

(define (head-ptr-dqi dqi) (car dqi))
(define (tail-ptr-dqi dqi) (cddr dqi))

(define (set-head-dqi! s-dqi t-dqi)
  (set-car! (head-ref-dqi s-dqi) t-dqi))

(define (set-tail-dqi! s-dqi t-dqi)
  (set-cdr! (tail-ref-dqi s-dqi) t-dqi))

(define dqi1 (new-deque-item 1))
(define dqi2 (new-deque-item 2))
(define dqi3 (new-deque-item 3))

;;; 테스트
;; dqi1 <-> dqi2 <-> dqi3
(set-head-dqi! dqi2 dqi1)
(set-tail-dqi! dqi2 dqi3)

dqi2

;;;--------------

(define (empty-deque? deque) (null? (front-deque-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (val-dqi (front-deque-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (val-dqi (rear-deque-ptr deque))))

;;        +------------------------- front
;;        |    3                     |
;;        V                          V
;;      +---+---+  +---+---+    2  +---+---+  +---+---+  
;; <----|   |   |->|   |   | <-----|   |   |->|   |   |  
;;      |   |   |  |   |   |------>|   |   |  |   |   |--->
;;      +---+---+  +---+---+   1   +---+---+  +---+---+  
;;                   |                          |
;;                   V                          V
;;                 +---+                      +---+
;;                 |   |                      |   |
;;                 |   |                      |   |
;;                 +---+                      +---+
;;      (new-deque-item)

(define (front-insert-deque! deque item)
  (let ((new-dqi (new-deque-item item)))
    (cond ((empty-deque? deque)
	   (set-front-deque! deque new-dqi)
	   (set-rear-ptr! deque new-dqi)
	   deque)
	  (else
	   (set-tail-dqi! new-dqi (front-deque-ptr deque))
	   (set-head-dqi! (front-deque-ptr deque) new-dqi)
	   (set-front-deque! deque new-dqi)
	   deque))))

;;                   rear ----------------------+
;;                   |        3                 |
;;                   V                          V
;;      +---+---+  +---+---+    1  +---+---+  +---+---+  
;; <----|   |   |->|   |   | <-----|   |   |->|   |   |  
;;      |   |   |  |   |   |------>|   |   |  |   |   |--->
;;      +---+---+  +---+---+   2   +---+---+  +---+---+  
;;                   |                          |
;;                   V                          V
;;                 +---+                      +---+
;;                 |   |                      |   |
;;                 |   |                      |   |
;;                 +---+                      +---+
;;                                 (new-deque-item)
(define (rear-insert-deque! deque item)
  (let ((new-dqi (new-deque-item item)))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-dqi)
	   (set-rear-ptr! deque new-dqi)
	   deque)
	  (else
	   (set-head-dqi! new-dqi (rear-deque-ptr deque))
	   (set-tail-dqi! (rear-deque-ptr deque) new-dqi)
	   (set-rear-deque! deque new-dqi)
	   deque))))


(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
;;	 (set-tail-dqi! (front-ptr deque) '())
	 (set-front-deque! deque (tail-ptr-dqi (front-ptr deque)))
	 deque)))


(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
;;	 (set-head-dqi! (rear-ptr deque) '())
	 (set-rear-deque! deque (head-ptr-dqi (rear-ptr deque)))
	 deque)))

(define (print-deque deque)
  (define (iter-print dqi)
    (if (not (eq? dqi (rear-ptr deque))) ;(null? dqi))
	(begin
	  (display (val-dqi dqi))
	  (display " ")
;;	  (display "<->")
	  (iter-print (tail-ptr-dqi dqi)))
	(begin
	  (display (val-dqi dqi))
	  (newline))))
  (iter-print (front-ptr deque)))


(print-deque dq1)

	  
	
;;;
(define dq1 (make-deque))

(front-insert-deque! dq1 'a)
(print-deque dq1)

(front-insert-deque! dq1 'b)
(print-deque dq1)

(rear-insert-deque! dq1 'c)
(print-deque dq1)

(rear-insert-deque! dq1 'd)
(print-deque dq1)

(front-delete-deque! dq1)
(print-deque dq1)

(rear-delete-deque! dq1)
(print-deque dq1)

(front-insert-deque! dq1 'e)
(print-deque dq1)

(rear-insert-deque! dq1 'f)
(print-deque dq1)


;;;==========================================
;;; 3.3.3 표
;;; p346






;;;==========================================
;;; 3.3.4 디지털 회로 시뮬레이터
;;; p354





;;;==========================================
;;; 3.3.5 관계 알리기
;;; p370

