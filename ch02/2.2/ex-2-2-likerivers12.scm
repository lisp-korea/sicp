;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 2 데이터를 요약해서 표현력을 끌어올리는 방법
;;; Ch 2.2 계층 구조 데이터와 닫힘 성질

;;;;=================<ch 2.2.1 차례열의 표현 방법>=====================
;;; p129

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

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)
;; 16


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


;;;--------------------------< ex 2.20 >--------------------------
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
;;                                   1)
;;              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                                   2)
;;      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                                   3)
;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                                   4)

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

