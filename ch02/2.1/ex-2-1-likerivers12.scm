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

(print-rat (make-rat 3 6))   ; ( 1 . 2) ->  1/2
(print-rat (make-rat -3 6))  ; (-1 . 2) -> -1/2
(print-rat (make-rat 3 -6))  ; (-1 . 2) -> -1/2
(print-rat (make-rat -3 -6)) ; ( 1 . 2) ->  1/2


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

(let ((N (+ 8 4667 15820 14287181)))
  (* (/ (+ 8 4667) N) (/ (+ 8 15820) N) N))


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
;;; p122
(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(let ((a (make-interval 4 6))
      (b (make-interval 9 11)))
  (display "interval a")
  (newline)
  (display a)
  (newline)
  (display "interval b")
  (newline)
  (display b)
  (newline)
  (display "add-interval")
  (newline)
  (display (add-interval a b))
  (newline)
  (display "mul-interval")
  (newline)
  (display (mul-interval a b))
  (newline)
  (display "div-interval")
  (newline)
  (display (div-interval a b))
  (newline))
	 

;;;--------------------------< ex 2.8 >--------------------------
;;; p122
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))


(let ((a (make-interval 4 6))
      (b (make-interval 9 11)))
  (display "interval a")
  (newline)
  (display a)
  (newline)
  (display "interval b")
  (newline)
  (display b)
  (newline)
  (display "add-interval")
  (newline)
  (display (add-interval a b))
  (newline)
  (display "mul-interval")
  (newline)
  (display (mul-interval a b))
  (newline)
  (display "div-interval")
  (newline)
  (display (div-interval a b))
  (newline)
  (display "sub-interval")
  (newline)
  (display (sub-interval a b))
  (newline))
	 

;;;--------------------------< ex 2.9 >--------------------------
;;; p123
(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i))
     2.0))

(define ia (make-interval 4 6))

(width-interval ia)

(define ib (make-interval 9 10))

(width-interval ib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a: -+--+--+--+--+--+--+--
;;;       [1     3]
;;;
;;; b: -+--+--+--+--+--+--+--
;;;                [4     6]
;;;=> a : (1 . 3)
;;;=> b : (4 . 6)

(define a (make-interval 1 3)) ; (1 . 3)

(define wa (width-interval a)) ; 1.0

(define b (make-interval 4 6)) ; (4 . 6)

(define wb (width-interval b)) ; 1.0

(define a+b (add-interval a b)) ; (5 . 9)

(define wa+b (width-interval a+b)) ; 2.0
;;= (+ (width-interval a) (width-interval b)) ; 2.0

(define a*b (mul-interval a b)) ; (4 . 18)

(define wa*b (width-interval a*b)) ; 7.0
;;!= (* (width-interval a) (width-interval b)) ; 1.0


;;;--------------------------< ex 2.10 >--------------------------


(define (div-interval x y)
  (let ((uy (upper-bound y))
	(ly (lower-bound y)))
    (if (and (not (= uy 0)) (not (= ly 0)))
	(mul-interval x
		      (make-interval (/ 1.0 uy)
				     (/ 1.0 ly)))
	(error "devide by zero!! rhs has a zero!"))))

(define a (make-interval 1 2))

(define b (make-interval 0 3))

(define c (make-interval 1 3))

(div-interval a b)

(div-interval a c)

;;;--------------------------< ex 2.11 >--------------------------
;;; 9가지 경우
;;; (- -) * { (- -), (- +), (+ +) } 
;;; (- +) * { (- -), (- +), (+ +) } 
;;; (+ +) * { (- -), (- +), (+ +) } 

;;; (a b) * (c d)
(mul-interval (make-interval -2 -1) (make-interval -4 -3)) ; ( 3  .  8) - (bd ac)
(mul-interval (make-interval -2 -1) (make-interval -4  3)) ; (-6  .  8) - (ad ac)
(mul-interval (make-interval -2 -1) (make-interval  3  4)) ; (-8  . -3) - (ad bc)

(mul-interval (make-interval -2  1) (make-interval -4 -3)) ; (-4  .  8) - (bc ac)
(mul-interval (make-interval -2  1) (make-interval -4  3)) ; (-6  .  8) - (ad ac)
(mul-interval (make-interval -2  1) (make-interval  3  4)) ; (-8  .  4) - (ad bd)

(mul-interval (make-interval  1  2) (make-interval -4 -3)) ; (-8  . -3) - (bc ad)
(mul-interval (make-interval  1  2) (make-interval -4  3)) ; (-8  .  6) - (bc bd)
(mul-interval (make-interval  1  2) (make-interval  3  4)) ; ( 3  .  8) - (ac bd)

(define (mul-interval2 x y)
  (let ((a (lower-bound x))
	(b (upper-bound x))
	(c (lower-bound y))
	(d (upper-bound y)))
    (cond ((and (< a 0) (< b 0))
	   (cond ((and (< c 0) (< d 0)) (make-interval (* b d) (* a c)))
		 ((and (< c 0) (> d 0)) (make-interval (* a d) (* a c)))
		 ((and (> c 0) (> d 0)) (make-interval (* a d) (* b c)))))
	  ((and (< a 0) (> b 0))
	   (cond ((and (< c 0) (< d 0)) (make-interval (* b c) (* a c)))
		 ((and (< c 0) (> d 0)) (make-interval (* a d) (* a c)))
		 ((and (> c 0) (> d 0)) (make-interval (* a d) (* b d)))))
	  ((and (> a 0) (> b 0))
	   (cond ((and (< c 0) (< d 0)) (make-interval (* b c) (* a d)))
		 ((and (< c 0) (> d 0)) (make-interval (* b c) (* b d)))
		 ((and (> c 0) (> d 0)) (make-interval (* a c) (* b d))))))))

(mul-interval2 (make-interval -2 -1) (make-interval -4 -3)) ; ( 3  .  8) - (bd ac)
(mul-interval2 (make-interval -2 -1) (make-interval -4  3)) ; (-6  .  8) - (ad ac)
(mul-interval2 (make-interval -2 -1) (make-interval  3  4)) ; (-8  . -3) - (ad bc)

(mul-interval2 (make-interval -2  1) (make-interval -4 -3)) ; (-4  .  8) - (bc ac)
(mul-interval2 (make-interval -2  1) (make-interval -4  3)) ; (-6  .  8) - (ad ac)
(mul-interval2 (make-interval -2  1) (make-interval  3  4)) ; (-8  .  4) - (ad bd)

(mul-interval2 (make-interval  1  2) (make-interval -4 -3)) ; (-8  . -3) - (bc ad)
(mul-interval2 (make-interval  1  2) (make-interval -4  3)) ; (-8  .  6) - (bc bd)
(mul-interval2 (make-interval  1  2) (make-interval  3  4)) ; ( 3  .  8) - (ac bd)




;;; p123,124
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(make-center-width 10 0.5) ; (9.5 . 10.5)
(center (make-center-width 10 0.5)) ; 10.0
(width (make-center-width 10 0.5)) ; 0.5


;;;--------------------------< ex 2.12 >--------------------------
;;; 주의! c 가 음수일 때
(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (if (> c 0)
	(make-center-width c w)
	(make-center-width c (- w)))))

(define (percent i)
  (let ((c (center i))
	(w (width i)))
    (* (/ w c) 100)))
  
(make-center-percent 100 10) ; (90 . 110)
(percent (make-center-percent 100 10)) ; 10

(make-center-percent 100 5) ; (95 . 105)
(percent (make-center-percent 100 5)) ; 5

(make-center-percent -100 10) ; (-110 . -90)


;;; 문제점 : 0이라면 오차율을 어떻게 적용하지!?



;;;--------------------------< ex 2.13 >--------------------------
;;; 가운데값과 허용 오차로 두 개의 구간을 표현하면
;;; i1 = (m a%) => (m-ma, m+ma)
;;; i2 = (n b%) => (n-na, n+na)
;;;
;;; 두 구간이 모두 양수라면
;;;
;;; i1*i2 =>
;;;
;;; lower-bound : 
;;;   (m-ma)*(n-na) 
;;; = (mn - mna - mnb + mnab)
;;; = mn * (1 - a - b + ab)
;;; 
;;; upper-bound :
;;;   (m+ma)*(n+na)
;;; = (mn + mna + mnb + mnab)
;;; = mn * (1 + a + b + ab)

(define (mul-interval-pos x y)
  (let ((m (center x))
	(a (/ (percent x) 100))
	(n (center y))
	(b (/ (percent y) 100)))
    (let ((lbf (+ 1 (- a) (- b) (* a b)))
	  (ubf (+ 1 a b (* a b))))
      (make-interval (* m n lbf) (* m n ubf)))))

(mul-interval (make-center-percent 1. 1)
	      (make-center-percent 2. 1))
; (1.9602 . 2.0402)


(mul-interval2 (make-center-percent 1. 1)
	      (make-center-percent 2. 1))
; (1.9602 . 2.0402)

(mul-interval-pos (make-center-percent 1. 1)
		  (make-center-percent 2. 1))
; (1.9602 . 2.0402)




;;; 구간값으로 음수가 가능하다면 잘못된 결과를 낼 수 있다.
(mul-interval (make-center-percent -1  1)
	      (make-center-percent 2. 1))
; (-2.0402 . -1.9602)

(mul-interval2 (make-center-percent -1  1)
	      (make-center-percent 2. 1))
; (-2.0402 . -1.9602)

(mul-interval-pos (make-center-percent -1. 1)
		  (make-center-percent 2. 1))
; (-1.9998 . -1.998)  <- 잘못된 결과





;;;p125
;;; R1*R2 / (R1 + R2)
;;; 1 / ( 1/R1 + 1/R2 )

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))



;;;--------------------------< ex 2.14 >--------------------------
(define P1 (par1 (make-center-percent 10 0.1)
		 (make-center-percent 20 0.1)))
; (6.646693306693306 . 6.686693360026694)
(center P1) ; 6.66669333336
(percent P1) ; 0.29999920000240643
(percent (make-center-percent 10 0.1))  ; 0.09999999999999788

(define P2 (par2 (make-center-percent 10 0.1)
		 (make-center-percent 20 0.1)))
; (6.66 . 6.673333333333333)
(center P2) ; 6.666666666666666
(percent P2) ; 0.09999999999999566
;; <- par2 방식의 경우 구간의 오차율이 입력 구간과 거의 같다

(define A (make-center-percent 10 0.1)) ; (9.99 . 10.01)
(center A) ; 10.0
(percent A) ; 0.09999999999999788

(define B (make-center-percent 20 0.1)) ; (19.98 . 20.02)
(center B) ; 20.0
(percent B) ; 0.09999999999999788

(define one-i (make-interval 1 1)) ; (1 . 1)


;;; A/A
;;; par1 방식
(define A/A-1 (div-interval A A)) ; (0.9980019980019981 . 1.002002002002002)
(center A/A-1) ; 1.000002000002
(percent A/A-1) ; 0.19999980000019435


;;; A/A = A * 1/A
;;; par2 방식
(define A/A-2 (mul-interval A 
			    (div-interval one-i A))) ; (0.9980019980019981 . 1.002002002002002)
;;; <= 두 방식이 같은 값을 내놓는다.

;;; A/B
;;; par1 방식
(define A/B-1 (div-interval A B)) ; (0.49900099900099903 . 0.501001001001001)
(center A/B-1) ; 0.500001000001
(percent A/B-1) ; 0.19999980000019435

(define A/B-2 (mul-interval A (div-interval one-i B))) ; (0.49900099900099903 . 0.501001001001001)

;;; <= 두 방식이 같은 값을 내놓는다.






;;; A + B
(define A+B (add-interval A B)) ; (29.97 . 30.03)
(center A+B) ; 30.0
(percent A+B) ; 0.1000000000000038

;;; A*B
(define A*B (mul-interval A B)) ; (199.6002 . 200.40019999999998)
(center A*B) ; 200.0002
(percent A*B) ; 0.19999980000019574

(div-interval A+B A*B) ; (0.14955074895134834 . 0.15045075105135164)
(mul-interval A+B (div-interval one-i A*B)) ; 위와 같음



;;; 1/A
(define 1/A (div-interval one-i A)) ; (0.0999000999000999 . 0.10010010010010009)
(center 1/A) ; 0.1000001000001
(percent 1/A) ; 0.09999999999999441

;;; 1/B
(define 1/B (div-interval one-i B)) ; (0.04995004995004995 . 0.050050050050050046)
(center 1/B) ; 0.05000005000005
(percent 1/B) ; 0.09999999999999441

(define 1/A+1/B (add-interval 1/A 1/B))
; (0.14985014985014986 . 0.15015015015015015)
;;<- (div-interval A+B A*B) 와 유사한 값을 가진다.

(mul-interval one-i 1/A+1/B) ; (0.14985014985014986 . 0.15015015015015015)





;;;--------------------------< ex 2.15 >--------------------------


;;;--------------------------< ex 2.16 >--------------------------


