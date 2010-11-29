;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 2 데이터를 요약해서 표현력을 끌어올리는 방법
;;; Ch 2.1  데이터 요약

;;;;=================<ch 2.1.1 연습 : 유리수를 위한 산술 연산>=====================
;;; p108

;;; p109
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))



;;; 유리수 만들기
;;; p111
(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))



;;; 테스트
(define one-half (make-rat 1 2))

(print-rat one-half)
; 1/2

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))
; 5/6

(print-rat (mul-rat one-half one-third))
; 1/6

(print-rat (add-rat one-third one-third))
; 6/9

;;;---
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;;;---

;; 기약분수로 만들기
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third))
; 2/3


;;;--------------------------< ex 2.1 >--------------------------
;;; p113
;;; 음수를 다룰 수 있는 make-rat

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((g-n (/ n g))
	  (g-d (/ d g)))
      (cond ((> (* g-n g-d) 0)
	     (cons (abs g-n) (abs g-d)))
	    (else
	     (cons (- (abs g-n)) (abs g-d)))))))

(print-rat (make-rat 3 6))   ; 1/2
(print-rat (make-rat -3 6))  ; -1/2
(print-rat (make-rat 3 -6))  ; -1/2
(print-rat (make-rat -3 -6)) ; 1/2


;;;;=================<ch 2.1.2 요약의 경계>=====================
;;; p113

;;; p115
;;; 유리수를 짜맞출 때 분자와 분모를 약분하지 않고,
;;; 만들어 놓은 유리수에서 분자와 분모를 꺼낼 때 약분하도록 짜는 경우
(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))


;;;--------------------------< ex 2.2 >--------------------------
;;; p115,116
;;; 평면위의 선분을 표현
;;;  - 시작점, 끝점

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment seg)
  (let ((sp (start-segment seg))
	(ep (end-segment seg)))
    (let ((mp-x (/ (+ (x-point sp)
		      (x-point ep)) 2))
	  (mp-y (/ (+ (y-point sp)
		      (y-point ep)) 2)))
      (make-point mp-x mp-y))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define seg1 (make-segment (make-point 0 0) (make-point 1 2)))
; (0,0)-(1,2)
; -> mid-point : (0.5, 1)

(print-point (midpoint-segment seg1))
; (1/2, 1)


;;;--------------------------< ex 2.3 >--------------------------
;;; 평면의 네모꼴(직사각형)을 나타내는 데이터

;;     p1    seg1    p2
;;      +------------+
;; seg4 |            | seg2
;;      +------------+
;;     p4    seg3    p3

;; p1과 p3로 rectangle 표현
(define (make-rectangle p1 p3)
  (cons p1 p3))

(define (perimeter-rectangle rect)
  (let ((seg1 (seg1-rectangle rect))
	(seg2 (seg2-rectangle rect))
	(seg3 (seg3-rectangle rect))
	(seg4 (seg4-rectangle rect)))
    (let ((l1 (length-segment seg1))
	  (l2 (length-segment seg2))
	  (l3 (length-segment seg3))
	  (l4 (length-segment seg4)))
      (+ l1 l2 l3 l4))))

(define (area-rectangle rect)
  (let ((seg1 (seg1-rectangle rect))
	(seg2 (seg2-rectangle rect)))
    (let ((l1 (length-segment seg1))
	  (l2 (length-segment seg2)))
      (* l1 l2))))


(define (print-rectangle rect)
  (let ((p1 (p1-rectangle rect))
	(p2 (p2-rectangle rect))
	(p3 (p3-rectangle rect))
	(p4 (p4-rectangle rect)))
    (display "(")
    (print-point p1)
    (display ",")
    (print-point p2)
    (display ",")
    (print-point p3)
    (display ",")
    (print-point p4)
    (display ")")
    (newline)))


;;  (0,1)    seg1    p2
;;      +------------+
;; seg4 |            | seg2
;;      +------------+
;;     p4    seg3    (2,0)
(define rect1 (make-rectangle (make-point 0 1) (make-point 2 0)))

(perimeter-rectangle rect1) ; 6
(area-rectangle rect1)      ; 2

(print-rectangle rect1)
;; (
;; (0,1)
;; ,
;; (2,1)
;; ,
;; (2,0)
;; ,
;; (0,0)
;; )

(seg1-rectangle rect1)
(seg2-rectangle rect1)
(seg3-rectangle rect1)
(seg4-rectangle rect1)

;; [[---- 세부 구현 1
(define (p1-rectangle rect)
  (car rect))

(define (p2-rectangle rect)
  (let ((p1 (p1-rectangle rect))
	(p3 (p3-rectangle rect)))
    (let ((p1-y (y-point p1))
	  (p3-x (x-point p3)))
      (make-point p3-x p1-y))))

(define (p3-rectangle rect)
  (cdr rect))

(define (p4-rectangle rect)
  (let ((p1 (p1-rectangle rect))
	(p3 (p3-rectangle rect)))
    (let ((p1-x (x-point p1))
	  (p3-y (y-point p3)))
      (make-point p1-x p3-y))))
      
(define (seg1-rectangle rect)
  (let ((p1 (p1-rectangle rect))
	(p2 (p2-rectangle rect)))
    (make-segment p1 p2)))

(define (seg2-rectangle rect)
  (let ((p2 (p2-rectangle rect))
	(p3 (p3-rectangle rect)))
    (make-segment p2 p3)))

(define (seg3-rectangle rect)
  (let ((p3 (p3-rectangle rect))
	(p4 (p4-rectangle rect)))
    (make-segment p3 p4)))

(define (seg4-rectangle rect)
  (let ((p4 (p4-rectangle rect))
	(p1 (p1-rectangle rect)))
    (make-segment p4 p1)))


;;;---
(define (square x)
  (* x x))
;;;---

(define (length-segment seg)
  (let ((sp (start-segment seg))
	(ep (end-segment seg)))
    (sqrt (+ (square (- (x-point sp)
			(x-point ep)))
	     (square (- (y-point sp)
			(y-point ep)))))))

(length-segment (make-segment (make-point 0 0)
			      (make-point 3 4)))
; 5
;; 세부 구현 1 -------]]


;;------------------------------------------
;; 다른 구현에 따른 결과

(define rect2 (make-rectangle (make-point 0 1) (make-point 2 1)
			      (make-point 2 0) (make-point 0 0)))

;; perimeter-rectangle 과 area-rectangle 을 수정할 필요 없다.

(perimeter-rectangle rect2) ; 6
(area-rectangle rect2)      ; 2

(print-rectangle rect2)
;; (
;; (0,1)
;; ,
;; (2,1)
;; ,
;; (2,0)
;; ,
;; (0,0)
;; )

(seg1-rectangle rect2)
(seg2-rectangle rect2)
(seg3-rectangle rect2)
(seg4-rectangle rect2)

;; [[----- 세부 구현 2 

;;     p1    seg1    p2
;;      +------------+
;; seg4 |            | seg2
;;      +------------+
;;     p4    seg3    p3

;; p1, p2, p3, p4 로 rectangle 표현
(define (make-rectangle p1 p2 p3 p4)
  (cons (cons (make-segment p1 p2)    ; seg1
	      (make-segment p2 p3))   ; seg2 
	(cons (make-segment p3 p4)    ; seg3
	      (make-segment p4 p1)))) ; seg4

(define (seg1-rectangle rect)
  (car (car rect)))

(define (seg2-rectangle rect)
  (cdr (car rect)))

(define (seg3-rectangle rect)
  (car (cdr rect)))

(define (seg4-rectangle rect)
  (cdr (cdr rect)))

(define (p1-rectangle rect)
  (let ((seg1 (seg1-rectangle rect)))
    (start-segment seg1)))

(define (p2-rectangle rect)
  (let ((seg2 (seg2-rectangle rect)))
    (start-segment seg2)))

(define (p3-rectangle rect)
  (let ((seg3 (seg3-rectangle rect)))
    (start-segment seg3)))

(define (p4-rectangle rect)
  (let ((seg4 (seg4-rectangle rect)))
    (start-segment seg4)))



;;;;=================<ch 2.1.3 데이터란 무엇인가>=====================
;;; p116

;; p118
(define (cons-new x y)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)
;; cons-new 가 리턴하는 것은 x y 상태를 갖고 있는 distpatch 함수이다.
;; 즉 lexical closure를 리턴한다.

(define (car-new z) (z 0))

(define (cdr-new z) (z 1))

(car-new (cons-new 1 2)) ; 1
(cdr-new (cons-new 1 2)) ; 2

;; 프로시저를 물체처럼 다루는 힘을 갖추면 그로부터 합친 데이터를 나타내는 표현력이 절로 생겨난다.

;;;--------------------------< ex 2.4 >--------------------------
;;; p119

(define (cons-new x y)
  (lambda (m) (m x y)))

(define (car-new z)
  (z (lambda (p q) p)))

;;; car 동작 풀이
(car-new (cons-new 1 2)) ;->
(car-new (lambda (m) (m 1 2))) ;->
((lambda (m) (m 1 2)) (lambda (p q) p)) ;->
((lambda (p q) p) 1 2) ;->
;-> 1

;;;; cdr 정의
(define (cdr-new z)
  (z (lambda (p q) q)))

;;; cdr 동작 풀이 
(cdr-new (cons-new 1 2)) ;->
(cdr-new (lambda (m) (m 1 2))) ;->
((lambda (m) (m 1 2)) (lambda (p q) q)) ;->
((lambda (p q) q) 1 2) ;->
;-> 2


;;;--------------------------< ex 2.5 >--------------------------
;;; p120
;;; 산술연산만으로 양의 정수 쌍을 표현
;;; 정수 a,b 쌍을 2^a 3^b 로 나타낼 때 cons, car, cdr 정의
;;; (2와 3이 서로소이기 때문에 가능하다.)

;;; 2^a 을 head
;;; 3^b 을 tail 로 생각하고 계산해보자
(define (cons-new a b)
  (define (calc-head x)
    (if (= x 0)
	1
	(* 2 (calc-head (- x 1)))))
  (define (calc-tail x)
    (if (= x 0)
	1
	(* 3 (calc-tail (- x 1)))))
  (* (calc-head a)
     (calc-tail b)))

;;; c를 3으로 계속 나누면 나머지가 2^a 이 된다.
;;; 2^a 에서 2로 나눠지는 횟수를 구하면 a가 나온다
(define (car-new c)
  (define (num-head x)
    (let ((rem1 (remainder x 3)))
      (if (= rem1 0)
	  (num-head (/ x 3))
	  x)))
  (define (count-div-2 x)
    (let ((rem1 (remainder x 2)))
      (if (= rem1 0)
	  (+ 1 (count-div-2 (/ x 2)))
	  0)))
  (count-div-2 (num-head c)))

;;; c를 2로 계속 나누면 나머지가 3^b 이 된다.
;;; 3^b 에서 3으로 나눠지는 횟수를 구하면 b가 나온다
(define (cdr-new c)
  (define (num-tail x)
    (let ((rem1 (remainder x 2)))
      (if (= rem1 0)
	  (num-tail (/ x 2))
	  x)))
  (define (count-div-3 x)
    (let ((rem1 (remainder x 3)))
      (if (= rem1 0)
	  (+ 1 (count-div-3 (/ x 3)))
	  0)))
  (count-div-3 (num-tail c)))


(cons-new 4 5) ; 2^4 * 3^5 = 3888
(car-new (cons-new 4 5)) ; 4
(cdr-new (cons-new 4 5)) ; 5

;;;--------------------------------------
;;; num-tail, num-head : remainder-n 으로 일반화
;;; count-div-2, count-div-3 : count-div-n 으로 일반화
;;; cons-new 는 그대로

(define (car-new c)
  (count-div-n (remainder-n c 3) c) n))

(define (cdr-new c)
  (count-div-n (remainder-n c 2) 3))

;;; x = p*n + q 일 때 q를 찾아준다. (p,n,q는 양의 정수, q는 n으로 나누어 떨어지지 않음)
(define (remainder-n x n)
  (let ((rem1 (remainder x n)))
    (if (= rem1 0)
	(remainder-n (/ x n) n)
	x)))

;;; x = k^n 일 때 k를 찾아준다. (x, k, n 은 양의 정수)
(define (count-div-n x n)
  (let ((rem1 (remainder x n)))
    (if (= rem1 0)
	(+ 1 (count-div-n (/ x n) n))
	0)))

(cons-new 4 5) ; 2^4 * 3^5 = 3888
(car-new (cons-new 4 5)) ; 4
(cdr-new (cons-new 4 5)) ; 5


;;;--------------------------< ex 2.6 >--------------------------
;;; p120
;;;

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero) ;-> add-1 body
(lambda (f) (lambda (x) (f ((zero f) x)))) ;-> zero 펼치면
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) ;-> 4번째 f를 3번째 f에 적용
(lambda (f) (lambda (x) (f ((lambda (x) x) x)))) ;-> 4번째 x를 2번째 x에 적용
(lambda (f) (lambda (x) (f x))) ;끝


;;=> lambda (x) 의 body에서 f가 1번 적용됨
(define one (lambda (f) (lambda (x) (f x))))




(add-1 one) ;-> add-1 body
(lambda (f) (lambda (x) (f ((one f) x)))) ;-> one을 펼치면
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x)))) ;-> 5번째 f를 3번째 f에 적용
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x)))) ;-> 4번째 x를 2번째 x에 적용
(lambda (f) (lambda (x) (f (f x)))) ;끝


;;=> lambda (x) 의 body에서 f가 2번 적용됨
(define two (lambda (f) (lambda (x) (f (f x)))))





(one two) ;-> one을 펼치면
((lambda (f) (lambda (x) (f x)))  two) ;-> two 를 f에 적용
(lambda (x) (two x)) ;-> two를 펼치면
(lambda (x) ((lambda (f) (lambda (x) (f (f x)))) x)) ;-> 1번째 lambda (x) 에 bound 된 x를 z로 치환(바깥의 x와 안쪽의 x는 다른 것이므로)
(lambda (z) ((lambda (f) (lambda (x) (f (f x)))) z)) ;-> 2번째 z를 1번째 f에 적용
(lambda (z) (lambda (x) (z (z x)))) ;끝




(define (+new a b)
  (lambda (f) (lambda (x) (f (((a b) f) x)))))
;;                             ^^^^^
;;                             | (add-1 n) 에서 n이 적용되는 부분을 (+ a b)의 (a b) 로 나타내었다.


;;; one과 two를 더했으므로 three이고,
;;; 그 형태는 (lambda (f) (lambda (x) (f (f (f x))))) 가 될 것으로 예상됨.
(+new one two) ;-> +new 의 body
(lambda (f) (lambda (x) (f (((one two) f) x)))) ;-> (one two) 펼친 결과를 적용
(lambda (f) (lambda (x) (f (((lambda (z) (lambda (x) (z (z x)))) f) x)))) ; 3번째 f를 첫번째 z에 적용
(lambda (f) (lambda (x) (f ((lambda (x) (f (f x))) x)))) ; 4번째 x를 2번째 x에 적용
(lambda (f) (lambda (x) (f (f (f x))))) ; 끝 - 예상한 three 의 형태가 나왔다!!




;;;;=================<ch 2.1.4 집중 과제 : 구간 산술 연산 만들기>===================
;;; p120

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))
    


;;;--------------------------< ex 2.7 >--------------------------


;;;--------------------------< ex 2.8 >--------------------------


;;;--------------------------< ex 2.9 >--------------------------


;;;--------------------------< ex 2.10 >--------------------------


;;;--------------------------< ex 2.11 >--------------------------


;;;--------------------------< ex 2.12 >--------------------------


;;;--------------------------< ex 2.13 >--------------------------


;;;--------------------------< ex 2.14 >--------------------------


;;;--------------------------< ex 2.15 >--------------------------


;;;--------------------------< ex 2.16 >--------------------------


