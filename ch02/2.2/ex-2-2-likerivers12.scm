;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 2 데이터를 요약해서 표현력을 끌어올리는 방법
;;; Ch 2.2 계층 구조 데이터와 닫힘 성질

;;; p126

;;;-----------
;;; 쌍 만들기
;;; 1과 2의 쌍
(cons 1 2)
;; (1 . 2)

;;; 상자와 화살표로 나타내면.
;;; [ + | + ]
;;;   |   | 
;;;   1   2


;;; 쌍은 모든 종류의 데이터 구조를 짜맞추는데 두루 쓸 수 있다.


;;;-----------
;;; * 닫힘 성질
;;;  - 어떤 연산이 닫힘 성질을 가진다.
;;;    -> 연산으로 만든 물체가 다시 그 연산의 대상이 될 수 있음.


;;; 닫힘 성질을 이용한 자연수의 정의
;;; N -> 1
;;; N -> N V (N + 1)


;;; - 닫힘 성질은 계층 구조를 만들 수 있도록 해주기 때문에
;;;  데이터를 합치는 모든 표현 수단에서 표현력을 끌어올리는 열쇠가 된다.

;;;;=================<ch 2.2.1 차례열의 표현 방법>=====================
;;; p129

;;; 리스트 : cons를 겹쳐 만든 쌍의 차례열.
(cons 1
      (cons 2
	    (cons 3
		  (cons 4 '()))))
;; (1 2 3 4)


;;; 리스트에서 car, cdr, cons
;;; car : 리스트의 첫번째 원소를 골라내는 연산
;;; cdr : 리스트의 첫번째 원소를 뺀 나머지 리스트를 골라내는 연산
;;; cons : 원래 리스트의 맨 앞에 새 원소를 보태어 리스트로 만들어 내는 연산

;;; nil - '() : 원소를 가지지 않은 차례열(빈 리스트)

(define one-through-four (list 1 2 3 4))

;;; p130
(car one-through-four)
;; 1

(cdr one-through-four)
;; (2 3 4)

(car (cdr one-through-four))
;; 2

(cons 10 one-through-four)
;; (10 1 2 3 4)

(cons 5 one-through-four)
;; (5 1 2 3 4)

;;;-----------------------------
;;; 리스트 연산 
;;; p131

;;; 리스트의 n번째 원소를 내놓는 프로시저
;;; <규칙>
;;;  - n=0 이면 리스트의 car
;;;    그렇지 않으면 리스트의 cdr에서 n-1번째 원소.
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3) ;; 16

;;; null? : 빈리스트인지 아닌지 확인하는 기본 술어 프로시저


;;; 리스트의 원소가 몇 개인지 알아보는 프로시저
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(length odds)
;; 4

;;; 반복하는 length
(define (length items)
  (define (length-iter a count)
    (if (null? a)
	count
	(length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(length odds)
;; 4

(append squares odds)
;; (1 4 9 16 25 1 3 5 7)

(append odds squares)
;; (1 3 5 7 1 4 9 16 25)


;;; 되도는 append 정의
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append squares odds)
;; (1 4 9 16 25 1 3 5 7)

(append odds squares)
;; (1 3 5 7 1 4 9 16 25)

;;;--------------------------< ex 2.17 >--------------------------
;;; p133

(define (last-pair lst)
  (if (null? lst)
      '()
      (if (null? (cdr lst))
	  lst
	  (last-pair (cdr lst)))))

(last-pair (list 23 72 149 34))
;; (34)

(last-pair (list 1))
;; (1)

(last-pair '())
;; ()

;;;--------------------------< ex 2.18 >--------------------------
;;; p133

(define (reverse lst)
  (define (reverse-iter lst2 acc)
    (if (null? lst2)
	acc
	(reverse-iter (cdr lst2) (cons (car lst2) acc))))
  (reverse-iter lst '()))

(reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)


;;;--------------------------< ex 2.19 >--------------------------
;;; 동전 바꾸기 다시 보기
;;; p133,4

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))


(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(cc 100 us-coins)
;; 292

(cc 100 uk-coins)

(define us-coins (reverse (list 50 25 10 5 1)))

(cc 100 us-coins)
;; 292
;; 리스트 원소의 차례가 cc 프로시저의 결과에 영향을 주는가?
;; : 영향을 주지 않는다.
;;   coin-values 안에 있는 동전의 종류를 모두 고려하는데,
;;   리스트 원소의 순서가 바뀌는 것은 방법을 따져보는 순서가 바뀌는 것 뿐이다.


;;;--------------------------< ex 2.20 >--------------------------
;;; p134,5,6

(define (f x y . z) (list x y z))

(f 1 2 3 4 5 6)
;; (1 2 (3 4 5 6))

(define (g . w) w)

(g 1 2 3 4 5 6)
;; (1 2 3 4 5 6)

(define (same-parity . lst)
  (define (same-parity-inner predicate lst2)
    (if (null? lst2)
	'()
	(let ((el (car lst2)))
	  (if (predicate (car lst2))
	      (cons el (same-parity-inner predicate (cdr lst2)))
	      (same-parity-inner predicate (cdr lst2))))))
  (if (null? lst)
      '()
      (if (even? (car lst))
	  (same-parity-inner even? lst)
	  (same-parity-inner odd? lst))))

(same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)

(same-parity 2 3 4 5 6 7)
;; (2 4 6)

(same-parity )
;; ()

(same-parity 1 2 4 6)
;; (1)



;;;-----------------------------
;;; 리스트 매핑(mapping)
;;; p136

(define (scale-list items factor)
  (if (null? items)
      '() ;nil
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)
;; (10 20 30 40 50)

;;; p137
(define (map proc items)
  (if (null? items)
      '() ;nil
      (cons (proc (car items))
	    (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))
;; (10 2.5 11.6 17)

(map (lambda (x) (* x x))
     (list 1 2 3 4))
;; (1 4 9 16)


(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 1 2 3 4 5) 10)
;; (10 20 30 40 50)


;;;--------------------------< ex 2.21 >--------------------------
;;; p138
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))

;;;---
(define (square x)
  (* x x))
;;;---

(square-list (list 1 2 3 4))
;; (1 4 9 16)

(define (square-list items)
  (map (lambda (x) (square x)) items))

(square-list (list 1 2 3 4))
;; (1 4 9 16)


;;;--------------------------< ex 2.22 >--------------------------
;;; p138
;;; 반복하는 square-list

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items '())) ;;nil))

(square-list (list 1 2 3 4))
;; (16 9 4 1)

(cons (square 2) (cons (square 1) '()))  ; ...
;; items에서 뒤쪽에 있는 원소가 answer의 앞쪽에 cons된다.
;; [ + | + ]               <- 2)
;;   |   | 
;;   2   |
;;     [ + | + ]           <- 1)
;;       |   |
;;       1  nil


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items '()))

(square-list (list 1 2 3 4))
;; '((((() . 1) . 4) . 9) . 16)

;;; 1. (cons answer (...)) 에서 answer 가 atom이 아니라 cons cell 이다.
;;; 2. (cons answer (...)) 에서 (...)가 cons cell 이 아니라 atom 이다.
(cons '(1) 2)
;; ((1) . 2)


;;;; 수행 단계 분석
;; 1)
(cons '() 1)
;; [ + | + ]  <- new cons cell
;;   |   |
;;  nil  1
;;
;;-> (() . 1)

;; 2)
(define ans '(() . 1))

(cons ans 4)
;; [ + | + ] <- new cons cell
;;   |   |
;;  ans  4 
;;
;; => [ + | + ]
;;      |   |   
;;      |   4
;;      |
;;    [ + | + ]         <- ans
;;      |   |
;;     nil  1
;;
;;-> '((() . 1) . 4)

;; 3) 
(define ans '((() . 1) . 4))

(cons ans 9)
;; '(((() . 1) . 4) . 9)


;;; 원하는대로 답이 나오게 하려면
;;; a) 매번 list에 대한 append를 수행
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer
		      (list (square (car things)))))))
  (iter items '()))

(square-list (list 1 2 3 4))
;; (1 4 9 16)

(append (append (append (append '() (list 1)) (list 4)) (list 9)) (list 16))
;;                      ^^^^^^^^^^^^^^^^^^^^^
;;                             1) - (1)
;;              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                             2) - (1 4)
;;      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                             3) - (1 4 9)
;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                             4) - (1 4 9 16)

;;; b) 첫번째 방식으로 풀고 최종결과를 reverse
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (let ((res (iter items '())))
    (reverse res)))

(square-list (list 1 2 3 4))
;; (1 4 9 16)

;;;--------------------------< ex 2.23 >--------------------------
;;; p139

;; 풀이 1)
(define (for-each proc items)
  (if (null? items)
      '()
      (begin
	(proc (car items))
	(for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))
;; 57
;; 321
;; 88

;; 풀이 2)
(define (for-each proc items)
  (if (null? items)
      '()
      (let ((tmp (proc (car items))))
	(for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))
;; 57
;; 321
;; 88

;;;;=================<ch 2.2.2 계층 구조법>=====================
;;; p139
(cons (list 1 2) (list 3 4))
;; ((1 2) 3 4)

(define x (cons (list 1 2) (list 3 4)))

(length x)
;; 3

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(count-leaves x)
;; 4

(list x x)
;; '(((1 2) 3 4) ((1 2) 3 4))

(length (list x x))
;; 2

(count-leaves (list x x))
;; 8



;;;--------------------------< ex 2.24 >--------------------------
;;; p142
(list 1 (list 2 (list 3 4)))  ;에 대한 나무꼴
;; (1 (2 (3 4)))
;;=>
;; [ + | + ]-->[ + | / ]  
;;   |           | 
;;   1         [ + | + ]-->[ + | / ]
;;               |           |
;;               2         [ + | + ]--[ + | / ]
;;                           |          |
;;                           3          4

;;;--------------------------< ex 2.25 >--------------------------
;;; p142,3
;;; 7을 꺼집어 내려면
(car (cdaddr '(1 3 (5 7) 9)))
;; 7

(third
(caar '((7)))
;; 7

(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))
;; 7

;;;--------------------------< ex 2.26 >--------------------------
;;; p143

(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)
;; (1 2 3 4 5 6)

(cons x y)
;; ((1 2 3) 4 5 6)

(list x y)
;; ((1 2 3) (4 5 6))

;;;--------------------------< ex 2.27 >--------------------------
;;; p143,4
;((1 2) (3 4))
; ((3 4) (1 2))

;; ((4 3) (2 1))

;; 리스트가 아닌 경우에도 가능하도록 list인지 여부를 확인하는 내용을 추가함
(define (reverse items)
  (define (reverse-iter items2 acc)
    (if (list? items2)
	(if (null? items2)
	    acc
	    (reverse-iter (cdr items2) (cons (car items2) acc)))
	items2))
  (reverse-iter items '()))

(reverse '(1 2 3 4))
;; (4 3 2 1)

(reverse 1)
;; 1

(reverse '(1 . 2))
;; (1 . 2)

(reverse '(1 (2 . 3)))
;; ((2 . 3) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 풀이 1)
;; 인자가 항상 2 단계의 리스트라면 아래와 같이 해도 된다.
(define (deep-reverse items)
  (reverse (map reverse items)))

(deep-reverse '(1 2 3 4))
;; (4 3 2 1)

(deep-reverse '((1 2) (3 4)))
;; ((4 3) (2 1))

;; 그러나! 이 방법에서는 3단계 이상의 리스트에 대해서 내부의 순서를 바꾸지 못한다.
(deep-reverse '((1 2) (3 (4 5))))
;;-> (((4 5) 3) (2 1))
;; (((5 4) 3) (2 1)) 이 바른 답임.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 바른 풀이
(define (deep-reverse items)
  (cond ((null? items) '())
	((list? items) (reverse (map deep-reverse items)))
	(else items)))

(deep-reverse '(1 2 3 4))
;; (4 3 2 1)

(deep-reverse '((1 2) (3 4)))
;; ((4 3) (2 1))

(deep-reverse '((1 2) (3 (4 5))))
;; (((5 4) 3) (2 1))

(deep-reverse 1)
;; 1

(deep-reverse '(1 . 2))
;; (1 . 2)   <- 리스트가 아니므로 맞는 결과임.

(deep-reverse '(1 2 (3 4 (5 6)) 7 8 (9 . 10)))
;; ((9 . 10) 8 7 ((6 5) 4 3) 2 1)


(define x (list (list 1 2) (list 3 4)))

x
;; ((1 2) (3 4))

(reverse x)
;; ((3 4) (1 2))

(deep-reverse x)
;; ((4 3) (2 1))





;; 리스트 확인
(list? 1)             ; #f
(list? '())           ; #t
(list? '(1 . 2))      ; #f
(list? '(1 2))        ; #t
(list? '(1 . (2 3)))  ; #t

(pair? 1)             ; #f
(pair? '())           ; #f
(pair? '(1 . 2))      ; #t
(pair? '(1 2))        ; #t
(pair? '(1 . (2 3)))  ; #t

(define (my-list? x)
  (or (null? x) 
      (and (pair? x) (pair? (cdr x)))))

(my-list? 1)            ; #f
(my-list? '())          ; #t
(my-list? '(1 . 2))     ; #f
(my-list? '(1 2))       ; #t
(my-list? '(1 . (2 3))) ; #t

;;;--------------------------< ex 2.28 >--------------------------
;;; p144

;; 풀이 1)
(define (fringe items)
  (cond ((null? items) '())
	((list? items) (append (fringe (car items)) (fringe (cdr items))))
	(else (list items))))

(fringe '(1 2 3 4))
;; (1 2 3 4)

(fringe '((1 2) (3 4)))
;; (1 2 3 4)

(fringe '(1 2 (3 4 (5 6) 7) 8 9 (10 . 11) 12))
;; (1 2 3 4 5 6 7 8 9 (10 . 11) 12)

(define x (list (list 1 2) (list 3 4)))
x
;; ((1 2) (3 4))

(fringe x)
;; (1 2 3 4)

(fringe (list x x))
;; (1 2 3 4 1 2 3 4)


;;----------------------------------
;; 여러가지 시도...

;; 아래와 같이 하면..
(define (fringe items)
  (cond ((null? items) '())
	((list? items) (cons (fringe (car items)) (fringe (cdr items))))
	(else (list items))))
;;==
(define (fringe items)
  (cond ((null? items) '())
	((list? items) (append (map fringe items)))
	(else (list items))))

(fringe '(1 2 3 4))
;; ((1) (2) (3) (4))

(fringe '((1 2) (3 4)))
;; (((1) (2)) ((3) (4)))

(fringe '(1 2 (3 4 (5 6) 7) 8 9 (10 . 11) 12))
;; ((1) (2) ((3) (4) ((5) (6)) (7)) (8) (9) ((10 . 11)) (12))

;; 유사..
(define (fringe items)
  (cond ((null? items) '())
	((list? items) (cons (fringe (car items)) (cons (fringe (cdr items)) '())))
	(else items)))


(fringe '(1 2 3 4))
;; '(1 (2 (3 (4 ()))))

(fringe '((1 2) (3 4)))
;; '((1 (2 ())) ((3 (4 ())) ()))

(fringe '(1 2 (3 4 (5 6) 7) 8 9 (10 . 11) 12))
;; '(1 (2 ((3 (4 ((5 (6 ())) (7 ())))) (8 (9 ((10 . 11) (12 ())))))))

;;;--------------------------< ex 2.29 >--------------------------
;;; p144,5

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;-------------------------------------------------
;; a) - 모빌에서 가지를 골라내는 고르개 정의
;;    - 가지의 구성요소를 골라내는 고르개
(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

;;;--------------------------
;;; 테스트                
;;                       m0
;;                        *
;;                        |
;;                m1      |        m2
;;                 *-=----+----=---*
;;                 |               |
;;            m3   |   s4      m5  |       m6 
;;             *---+---6        *--+----=--*
;;             |                |          |
;;          s7 |  s8     s9     |  s10  s11|  s12
;;           4-+--2       2=----+--5    1--+--1


(define s12 1)
(define b12 (make-branch 2 s12))

(define s11 1)
(define b11 (make-branch 2 s11))

(define s10 5)
(define b10 (make-branch 2 s10))

(define s9 2)
(define b9 (make-branch 5 s9))

(define s8 2)
(define b8 (make-branch 2 s8))

(define s7 4)
(define b7 (make-branch 1 s7))

(define m6 (make-mobile b11 b12))
(define b6 (make-branch 7 m6))

(define m5 (make-mobile b9 b10))
(define b5 (make-branch 2 m5))

(define s4 6)
(define b4 (make-branch 3 s4))

(define m3 (make-mobile b7 b8))
(define b3 (make-branch 3 m3))

(define m2 (make-mobile b5 b6))
(define b2 (make-branch 8 m2))

(define m1 (make-mobile b3 b4))
(define b1 (make-branch 6 m1))

(define m0 (make-branch b1 b2))

m0 
;; '((6 ((3 ((1 4) (2 2))) (3 6))) (8 ((2 ((5 2) (2 5))) (7 ((2 1) (2 1))))))

m5
;; '((5 2) (2 5))

s10
;; 5

s4
;; 6

(left-branch m5)  ; '(5 2)
(right-branch m5) ; '(2 5)

(branch-length (right-branch m5)) ; 2
(branch-structure (right-branch m5)) ; 5

;;-------------------------------------------------
;; b) 모빌의 전체 무게

(define (total-weight m)
  (if (mobile? m)
      (let ((left (left-branch m))
	    (right (right-branch m)))
	(+ (total-branch-weight left)
	   (total-branch-weight right)))
      m))

(define (total-branch-weight b)
  (let ((s (branch-structure b)))
    (if (mobile? s) 
	(total-weight s)
	s)))

(define (mobile? s)
  (if (pair? s)
      #t
      #f))

(total-weight m1) ;; 12 

(total-weight m2) ;; 9

(total-weight m0) ;; 21

(total-weight s10) ;; 5

;;-------------------------------------------------
;; c) 균형 잡힌 상태
;;  1) 왼쪽 맨 윗가지의 돌림힘 = 오른쪽 맨 윗가지의 돌림힘
;;     돌림힘 = (막대 길이) * (막대에 매달린 추 무게 합)
;;  2) 가지마다 매달린 모든 부분 모빌도 균형 잡힌 상태

(define (mobile-torque m)
  (if (mobile? m)
      (let ((lb (left-branch m))
	    (rb (right-branch m)))
	(cons (branch-torque lb)
	      (branch-torque rb)))
      0))

(define (branch-torque b)
  (* (total-branch-weight b) 
     (branch-length b)))

(define (mobile-balanced? m)
  (if (mobile? m)
      (let ((lb (left-branch m))
	    (rb (right-branch m)))
	(and (= (branch-torque lb) (branch-torque rb))
	     (branch-balanced? lb)
	     (branch-balanced? rb)))
      #t))

(define (branch-balanced? b)
  (if (pair? b)
      (let ((s (branch-structure b)))
	(if (mobile? s)
	    (mobile-balanced? s)
	    #t))
      #t))

(mobile-torque m0)     ;; (72 . 72)

(mobile-balanced? m0)  ;; #t

(mobile-torque m1)     ;; (18 . 18)

(mobile-balanced? m1)  ;; #t

(mobile-torque m2)     ;; (14 . 14)

(mobile-balanced? m2)  ;; #t

(mobile-torque m6)     ;; (2 . 2)

(mobile-balanced? m6)  ;; #t

(mobile-torque s10)    ;; 0

(mobile-balanced? s10) ;; #t

s4 ;; 6

m1 ;;'((3 ((1 4) (2 2))) (3 6))

;; 모빌 수정
(define s4 4) ;; <------------ 6에서 4로 수정
(define b4 (make-branch 3 s4))

(define m3 (make-mobile b7 b8))
(define b3 (make-branch 3 m3))

(define m2 (make-mobile b5 b6))
(define b2 (make-branch 8 m2))

(define m1 (make-mobile b3 b4))
(define b1 (make-branch 6 m1))

(define m0 (make-branch b1 b2))

s4 ;; 4

(mobile-torque s4)    ;; 0 

;;!!!!!
(mobile-balanced? s10) ;; #t

;;;
(mobile-torque m0)     ;; (60 . 72)

(mobile-balanced? m0)  ;; #f

(mobile-torque m1)     ;; (18 . 12)

(mobile-balanced? m1)  ;; #f

(mobile-torque m2)     ;; (14 . 14)

(mobile-balanced? m2)  ;; #t

(mobile-torque m6)     ;; (2 . 2)

(mobile-balanced? m6)  ;; #t

(mobile-torque s10)    ;; 0

(mobile-balanced? s10) ;; #t

;;-------------------------------------------------
;; d) 짜맞추개를 바꾸면 지금까지 짠 프로그램을 얼마나 손봐야하나?

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;;;----------------------
;;; 수정 필요
(define (right-branch m)
  (cdr m)) ; <- cadr

(define (branch-structure b)
  (cdr b)) ; <- cadr
;;;----------------------




;;; 나무 매핑
;;; p145

(define (scale-tree tree factor)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
	    10)
;; (10 (20 (30 40) 50) (60 70))

;;; map 이용
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
	    10)
;; (10 (20 (30 40) 50) (60 70))

;;;--------------------------< ex 2.30 >--------------------------
;;; p146,7

;;;---
(define (square x) (* x x))

;; 곧 바로 정의
(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else
	 (cons (square-tree (car tree))
	       (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; 입력 : '(1 (2 (3 4) 5) (6 7))
;; 결과 : '(1 (4 (9 16) 25) (36 49))

;; map과 재귀를 써서 정의
(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; '(1 (4 (9 16) 25) (36 49))

;;;--------------------------< ex 2.31 >--------------------------
;;; p147

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; '(1 (4 (9 16) 25) (36 49))


;;;--------------------------< ex 2.32 >--------------------------
;;; p147

;; 영식님의 방법으로
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x))
			  rest)))))

(subsets '(1 2 3))
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(define (subsets s)
  (begin 
    (print "---->")
    (print s)
    (newline)
    (if (null? s)
	(list '())
	(let ((rest (subsets (cdr s))))
	  (print "s    :")
	  (print s)
	  (newline)
	  (print "rest :")
	  (print rest)
	  (newline)
	  (newline)
	  (append rest (map (lambda (x) (cons (car s) x))
			    rest))))))

(subsets '(1 2 3))
;; "---->"'(1 2 3)
;; "---->"'(2 3)
;; "---->"'(3)
;; "---->"'()
;; "s    :"'(3)
;; "rest :"'(())
;;
;; "s    :"'(2 3)
;; "rest :"'(() (3))
;;
;; "s    :"'(1 2 3)
;; "rest :"'(() (3) (2) (2 3))
;;
;; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


;;;;==============<ch 2.2.3 공통 인터페이스로써 차례열의 쓰임새>==================
;;; p147

;;;---
(define (square x) (* x x))
;;;---

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
	((not (pair? tree))
	 (if (odd? tree) (square tree) 0))
	(else (+ (sum-odd-squares (car tree))
		 (sum-odd-squares (cdr tree))))))

(sum-odd-squares '((1 2 (3 4)) ((5 6 7) (8 9 10))))
;; 3^2 + 5^2 + 7^2 + 9^2
;; 165

(define (even-fibs n)
  (define (next k)
    (if (> k n)
	'()
	(let ((f (fib k)))
	  (if (even? f)
	      (cons f (next (+ k 1)))
	      (next (+ k 1))))))
  (next 0))

;;;---
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else
	 (+ (fib (- n 1)) (fib (- n 2))))))
;;;---

(even-fibs 10) ; '(0 2 8 34)


;;; enumerate -> filter -> map -> accumulate
;;; enumerate -> map -> filter -> accumulate


;;;;;;;;;;;;;;;;;;;;;;
;;; 차례열 연산
;; p150

;;;---
(define nil '())
;;;---


;;; 매핑 단계 - map
;;; 골라내는 연산 - filter
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))  ; '(1 3 5)

;;; 어큐물례이쎤
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5)) ; 15

(accumulate * 1 (list 1 2 3 4 5)) ; 120

(accumulate cons nil (list 1 2 3 4 5)) ; '(1 2 3 4 5)


;;; 어떤 범위에 있는 모든 정수를 뽑아내는 연산
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7) ; '(2 3 4 5 6 7)


;;; 나무에서 잎사귀를 떼어낼 때
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
;; '(1 2 3 4 5)


;;; sum-odd-square를 다시 정의
(define (sum-odd-squares tree)
  (accumulate + 
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(sum-odd-squares '(1 2 3 4 5)) ; 35

;;; even-fibs를 다시 정의
(define (even-fibs n)
  (accumulate cons
	      nil
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

(even-fibs 10) ; '(0 2 8 34)


;;; p153
;;; 차례열 연산으로 프로그램을 표현하는 방법은 모듈 방식
;;; 곧 독립된 부품을 짜 맞추듯이 프로그램을 설계할 수 있다는 점에서 가치가 있다.

;;; 부품 재사용

(define (list-fib-squares n)
  (accumulate cons
	      nil
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))

(list-fib-squares 10) ; '(0 1 1 4 9 25 64 169 441 1156 3025)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
	      1
	      (map square
		   (filter odd? sequence))))

(product-of-squares-of-odd-elements '(1 2 3 4 5)) ; 225


;; (define (salary-of-highest-paid-programmer records)
;;   (accumulate max
;; 	      0
;; 	      (map salary
;; 		   (filter programmer? records))))


   

;;;--------------------------< ex 2.33 >--------------------------
;;; p155

(define (map2 p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y))   ;; <---
	      nil
	      sequence))

;;; accumulate 코드에서
;; (op       (car sequence) (accumulate op initial (cdr sequence)))
;; ^^^       ^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; lambda..    x                    y

(map square '(1 2 3 4))  ; '(1 4 9 16)
(map2 square '(1 2 3 4)) ; '(1 4 9 16)


;;;----------------
(define (append2 seq1 seq2)
  (accumulate cons 
	      seq2 ;;<--
	      seq1 ;;<--
	      ))

(append '(1 2) '(3 4)) ; '(1 2 3 4)
(append '(1) 2)        ; '(1 . 2)

(append2 '(1 2) '(3 4))  ; '(1 2 3 4)
(append2 '(1) 2)         ; '(1 . 2)


;;;----------------
(define (length2 sequence)
  (accumulate (lambda (x y)
		(+ 1 y))
	      0 sequence))

(length '(1 2 3 4)) ; 4

(length2 '(1 2 3 4)) ; 4


;;;--------------------------< ex 2.34 >--------------------------
;;; p155,6

;; a_n     * x^n     +
;; a_(n-1) * x^(n-1) +
;; ...
;; a_1     * x^1     +
;; a_0
;;
;;=> (호너의 규칙)
;;
;; ( ... (a_n*x + a_(n-1) )*x + ... + a_1)*x + a_0
;;
;;=>
;; a_0 + x*(a_1 + ... + x*( a_(n-1) + x*a_n) ... )

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff 
		   (* x higher-terms))) ;;<--
		0
		coefficient-sequence))

;; x=2일 때, 1 + 3x + 5x^3 + x^5

(horner-eval 2 (list 1 3 0 5 0 1)) ; 79
;; (+ 1 (* 2 3) (* 5 (* 2 2 2)) (* 2 2 2 2 2))
;;;; -> 79


;;;--------------------------< ex 2.35 >--------------------------
;;; p156
;;; 2.2.2절의 count-leaves 를 accumulate를 이용해서 정의

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define (count-leaves2 t)
  (accumulate  +
	       0
	       (map length t)))

;;항상 깊이가 2이고, 깊이 2에서만 leaf가 매달린다면 위와 같이 해도 된다.
(count-leaves2 '((1 2) (3 4) (5 6)))

;; 어떤 tree에도 대응.
(define (count-leaves2 t)
  (accumulate  +
	       0
	       (map (lambda (x)
		      (if (not (pair? x))
			  1
			  (count-leaves2 x)))
		    t)))
;; tree의 원소가 leaf이면 map에서 1을 카운트하고
;; tree의 원소가 다시 tree이면 count-leaves를 되돈다.
;; accumulate는 그냥 map에서 1의 수를 되돌면서 더하면 된다.

(count-leaves '((1 2) ((3 4 5) 6) 7)) ; 7

(count-leaves2 '((1 2) ((3 4 5) 6) 7)) ; 7

;;;--------------------
;;; 되도는 map 테스트 
(define (test-map t)
  (if (null? t)
      nil
      (map (lambda (x)
	     (if (not (pair? x))
		 1
		 (test-map x)))
	   t)))

(test-map '((1 2) ((3 4 5) 6) 7))
;; '((1 1) ((1 1 1) 1) 1)


;;;--------------------------< ex 2.36 >--------------------------
;;; p156

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(accumulate-n + 0 s) ; '(22 26 30)

;; 또 다른 accumulate-n
;; 각 차례열을 더한 차례열을 내놓는 프로시저
(define (another-accumulate-n op init seqs)
  (if (not (pair? seqs))
      nil
      (if (null? (car seqs))
	  nil
	  (cons (accumulate op init (car seqs))
		(another-accumulate-n op init (cdr seqs))))))

(another-accumulate-n + 0 s)
;; '(6 15 24 33)

;;;--------------------------< ex 2.37 >--------------------------
;;; p157

;;; 행렬
;;; ((1 2 3 4) (4 5 6 6) (6 7 8 9))
;;=>
;; [ 1 2 3 4
;;   4 5 6 6
;;   6 7 8 9 ]

;; (dot-product v w)
;; S_i : vi * wi

;; (matrix-*-vector m v)
;; ti = Sj : mij * vj

;; (matrix-*-matrix m n)
;; Pij = Sk : mik * nkj

;; (transpose m)
;; nij = mji

(define m '((1 2) (3 4)))

(define v '(5 6))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
;;; 2.2.1 에서 정의한 map으로는 안됨.

(dot-product v v) ; 61
;; (5 6) .* (5 6) = 5*5 + 6*6 = 61



(define (matrix-*-vector m v)
  (map (lambda (x)
	 (accumulate + 0 (map * x v)))
       m))

(matrix-*-vector m v) ; '(17 39)
;; [ 1 2 | * [ 5 | = [ 1*5 + 2*6 | = [ 17 |
;; | 3 4 ]   | 6 ]   | 3*5 + 4*6 ]   | 39 ]



(define (transpose mat)
  (accumulate-n cons nil mat))

(define m2 '((1 2 3) (4 5 6) (7 8 9)))

(transpose m2) ; '((1 4 7) (2 5 8) (3 6 9))
;; [ 1 2 3 | -> [ 1 4 7 |
;; | 4 5 6 |    | 2 5 8 |
;; | 7 8 9 ]    | 3 6 9 ]



(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) 
	   (accumulate cons nil (matrix-*-vector cols x)))
	 m)))

(matrix-*-matrix m m) ; '((7 10) (15 22))
;; [ 1 2 | * [ 1 2 | = [ 1*1 + 2*3   1*2 + 2*4 | = [  7 10 |
;; | 3 4 ]   | 3 4 ]   | 3*1 + 4*3   3*2 + 4*4 ]   | 15 22 ]

(matrix-*-matrix m2 m2) ; '((30 36 42) (66 81 96) (102 126 150))




;;;--------------------------< ex 2.38 >--------------------------
;;; p158

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3)) ; 3/2
;; (/ 1 (/ 2 (/ 3 1)))
;;                i
;;
;;   1     2     3
;;   |     |     |    
;;   |     |     /--> 1
;;   |     |          | (3/1) 
;;   |     |          v
;;   |     /------->  3
;;   |                | (2 / 3)
;;   |                v
;;   /------------ > 2/3
;;                    | (1 / (2/3))
;;                    v
;;                   3/2

(fold-right / 1 (list 4 5 6)) ; 24/5
;; (/ 4 (/ 5 (/ 6 1)))

(fold-left / 1 (list 1 2 3)) ; 1/6
;; (/ (/ (/ 1 1) 2) 3)
;;          i
;;          
;;                    1     2     3
;;                    |     |     |
;;             1  <-- /     |     |
;; (1 / 1)     |            |     |    
;;             v            |     |
;;             1  <-------- /     |  
;; (1 / 2)     |                  |  
;;             v                  |
;;            1/2 <-------------- /
;; (1/2) / 3   |
;;             v
;;            1/6

(fold-left / 1 (list 4 5 6)) ; 1/120
;; (/ (/ (/ 1 4) 5) 6)

(fold-right list nil (list 1 2 3)) ; '(1 (2 (3 ())))

(fold-left list nil (list 1 2 3)) ; '(((() 1) 2) 3)


;; 위에서 볼 때 
;; right에서는 2 3 -> (x / 2) / 3
;; left에서는  2 3 -> (y / 3) / 2
;; 즉 2,3의 순서가 바뀐다.
;; 같으려면 순서가 바껴도 연산의 결과가 같아야 한다 : 즉 교환 법칙 성립

(fold-left * 1 (list 4 5 6))  ; 120
(fold-right * 1 (list 4 5 6)) ; 120

(fold-left + 1 (list 4 5 6))  ; 16
(fold-right + 1 (list 4 5 6)) ; 16


;;;--------------------------< ex 2.39 >--------------------------
;;; p159

(define (reverse2 sequence)
  ;; initial 값이 y에 들어간다.
  ;; 중간 결과물이 y에 들어간다.
  ;; sequence의 원소는 뒤에서부터 x에 들어간다
  (fold-right (lambda (x y) 
		(append y (cons x nil))) 
	      nil 
	      sequence))

(reverse2 '(1 2 3 4))

(define (reverse2 sequence)
  ;; initial 값이 x에 들어간다.
  ;; 중간 결과물이 x에 들어간다.
  ;; sequence의 원소는 앞에서부터 y에 들어간다.
  (fold-left (lambda (x y) 
	       (cons y x))
	     nil 
	     sequence))

(reverse2 '(1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;
;;; 겹친 매핑
;; p159

;;(1) (enumerate-interval 1 n) 으로 차례열 뽑아내기
;;(2) 원소 i마다 (enumerate-interval 1 (- i 1)) 을 적용하여 다시 차례열 뽑기
;;(3) 차례열 원소 j에 대하여  (list i j) 만들기
;; (accumulate append
;; 	    nil
;; 	    (map (lambda (i)
;; 		   (map (lambda (j) (list i j))
;; 			(enumerate-interval 1 (- i 1))))
;; 		 (enumerate-interval 1 n)))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;;; 한데 엮으면 ->
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

(prime-sum-pairs 6)
;; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

(let ((n 4))
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))
;; '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))

;;;---
;; p64 ch 1.2.6
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;;;---


;;; p161
;;; 순열

;;(1) S의 각 원소 x에 대하셔, S - x 의 순열을 모두 구해 차례열로 묶어낸다.
;;(2) 차례열 속에 있는 각 순열의 맨 앞에 x를 덧붙인다.
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

(permutations '(1 2 3))
;; '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))


;;;--------------------------< ex 2.40 >--------------------------
;;; p162

;; 정수 n을 인자로 받아서 1<= j < i <= n 을 만족하는 (i,j) 쌍의 차례열 뽑기
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 2 n)))

(unique-pairs 5)
;; '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

;;; unique-pairs를 이용해서 prime-sum-pairs 정의 줄이기
(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))
	     
(prime-sum-pairs 5)
;; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))

(prime-sum-pairs2 5)
;; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))

;;;--------------------------< ex 2.41 >--------------------------
;;; p162

(define (unique-triple n)
  (define (unique-triple? items)
    (let ((a (car items))
  	  (b (cadr items))
  	  (c (caddr items)))
      (cond ((= a b) #f)
  	    ((= b c) #f)
  	    ((= a c) #f)
  	    (else #t))))
  (filter unique-triple?
	  (accumulate append nil
		      (flatmap (lambda (i)
				 (map (lambda (j)
					(map (lambda (k)
					       (list i j k))
					     (enumerate-interval 1 n)))
				      (enumerate-interval 1 n)))
			       (enumerate-interval 1 n)))))

(unique-triple 3)
;; '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

(unique-triple 4)
;; '((1 2 3)
;;   (1 2 4)
;;   (1 3 2)
;;   (1 3 4)
;;   (1 4 2)
;;   (1 4 3)
;;   (2 1 3)
;;   (2 1 4)
;;   (2 3 1)
;;   (2 3 4)
;;   (2 4 1)
;;   (2 4 3)
;;   (3 1 2)
;;   (3 1 4)
;;   (3 2 1)
;;   (3 2 4)
;;   (3 4 1)
;;   (3 4 2)
;;   (4 1 2)
;;   (4 1 3)
;;   (4 2 1)
;;   (4 2 3)
;;   (4 3 1)
;;   (4 3 2))


(define (unique-triple-sum n s)
  (filter (lambda (x) (= s (accumulate + 0 x)))
	  (unique-triple n)))

(unique-triple-sum 4 7)
;; '((1 2 4) (1 4 2) (2 1 4) (2 4 1) (4 1 2) (4 2 1))

;;;--------------------------< ex 2.42 >--------------------------
;;; p162,3,4

