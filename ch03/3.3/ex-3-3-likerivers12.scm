
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


;;;--------------------------< ex 3.18 >--------------------------


;;;--------------------------< ex 3.19 >--------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 변했다는 말은 그저 덮어썼다는 뜻이다.





;;;--------------------------< ex 3.20 >--------------------------





;;;==========================================
;;; 3.3.2 큐
;;; p339






;;;==========================================
;;; 3.3.3 표
;;; p346






;;;==========================================
;;; 3.3.4 디지털 회로 시뮬레이터
;;; p354





;;;==========================================
;;; 3.3.5 관계 알리기
;;; p370

