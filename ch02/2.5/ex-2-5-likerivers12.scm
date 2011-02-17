;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 2 데이터를 요약해서 표현력을 끌어올리는 방법
;;; Ch 2.5. 일반화된 연산 시스템
;;; p 244

;;; 한 데이터의 표현이 서로 다를 때,
;;; 인자마다 종류가 다은 경우
;;; 모두 가능한 연산 정의 방법.


;;;;=================<ch 2.5.1 일반화된 산술 연산>=====================
;;; p245

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;; p246
;;; ordinary(보통 수)

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  ;; ex 2.79
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y)
	 (= x y)))

  ;; ex 2.80
  (put 'zero? '(scheme-number)
       (lambda (x)
	 (= x 0)))

  ;; ex 2.81-a 
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))

  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;;; p247
;;; rational

(define (install-rational-package)
  ;; 갇힌 프로시저
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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

  ;; 인터페이스
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  ;; ex 2.79
  (define (equ? r1 r2)
    (and (= (numer r1) (numer r2))
	 (= (denom r1) (denom r2))))
  (put 'equ? '(rational rational)
       (lambda (r1 r2)
	 (equ? r1 r2)))

  ;; ex 2.80
  (put 'zero? '(rational)
       (lambda (x)
	 (= (numer x) 0)))

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;; p248
;;; complex
;; 뒤에서 보완
;; (define (install-complex-package)
;;   ;; 직각 좌표, 극좌표 꾸러미
;;   (define (make-from-real-imag x y)
;;     ((get 'make-from-real-imag 'rectangular) x y))
;;   (define (make-from-mag-ang r a)
;;     ((get 'make-from-mag-ang 'polar) r a))

;;   ;; 갇힌 프로시저
;;   (define (add-complex z1 z2)
;;     (make-from-real-imag (+ (real-part z1) (real-part z2))
;; 			 (+ (imag-part z1) (imag-part z2))))
;;   (define (sub-complex z1 z2)
;;     (make-from-real-imag (- (real-part z1) (real-part z2))
;; 			 (- (imag-part z1) (imag-part z2))))
;;   (define (mul-complex z1 z2)
;;     (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;; 		       (+ (angle z1) (angle z2))))
;;   (define (div-complex z1 z2)
;;     (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;; 		       (- (angle z1) (angle z2))))

;;   ;; 인터페이스
;;   (define (tag z) (attach-tag 'complex z))
;;   (put 'add '(complex complex)
;;        (lambda (z1 z2) (tag (add-complex z1 z2))))
;;   (put 'sub '(complex complex)
;;        (lambda (z1 z2) (tag (sub-complex z1 z2))))
;;   (put 'mul '(complex complex)
;;        (lambda (z1 z2) (tag (mul-complex z1 z2))))
;;   (put 'div '(complex complex)
;;        (lambda (z1 z2) (tag (div-complex z1 z2))))
;;   (put 'make-from-real-imag 'complex
;;        (lambda (x y) (tag (make-from-real-imag x y))))
;;   (put 'make-from-mag-ang 'complex
;;        (lambda (r a) (tag (make-from-mag-ang r a))))

;;   ;; ex 2.79
;;   (put 'equ? '(complex complex)
;;        (lambda (z1 z2) 
;; 	 (apply-generic 'equ? z1 z2)))

;;   ;; ex 2.80
;;   (put 'zero? '(complex)
;;        (lambda (x)
;; 	 (apply-generic 'zero? x)))

;;   'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;;;--------------------------< ex 2.77 >--------------------------
;;; 250

;;; 아래를 포함해야함
;;----------------------------------------------------------
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 나의 put,get 구현
(define GOP (make-hash))

(define (put op types behavior)
  (hash-set! GOP (cons op types) behavior))
;;  (hash-set! GOP (list op types) behavior))
;;  (hash-set! GOP (cons 'key (cons op types)) behavior))

(define (get op types)
  (hash-ref GOP (cons op types)))
;;  (hash-ref GOP (list op types)))
;;  (hash-ref GOP (cons 'key (cons op types))))

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

;;------------
;; p229
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
;;------------

;;--------------------------------
;; 여기서 타입 태그를 떼어낸다.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types -- APPLY-GENERTIC"
	   (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
;;--------------------------------

;;
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


;; ;;--------------------------------
;; (define sn1 ((get 'make 'scheme-number) 1))
;; (define sn2 ((get 'make 'scheme-number) 2))

;; ((get 'add '(scheme-number scheme-number)
;;       sn1 sn2))
;; ;;--------------------------------

(define (install-rectangular-package)
  ;; 갇힌 프로시저
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; 이 꾸러미의 인터페이스
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;; ex 2.79
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
	 (= (imag-part z1) (imag-part z2))))
  (put 'equ? '(rectangular rectangular)
       (lambda (z1 z2) 
	 (equ? z1 z2)))

  ;; ex 2.80
  (put 'zero? '(rectangular)
       (lambda (x)
       	 (and (= (real-part x) 0)
       	      (= (imag-part x) 0))))
;;       (lambda (x) (print "rectangular!")))

  'done)


;;; 극좌표 방식
(define (install-polar-package)
  ;; 갇힌 프로시저
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))

  (define (real-part z)
    (* (magnitude z) (cons (angle z))))
  (define (mag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))

  ;; 이 꾸러미의 인터페이스
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))


  ;; ex 2.79
  (define (equ? z1 z2)
    (and (= (magnitude z1) (magnitude z2))
	 (= (angle z1) (angle z2))))
  (put 'equ? '(polar polar)
       (lambda (z1 z2) 
	 (equ? z1 z2)))

  ;; ex 2.80
  (put 'zero? '(polar)
       (lambda (x)
	 (= (magnitude x) 0)))

  'done)

;;
;;----------------------------------------------------------

(define (install-complex-package)
  ;; 직각 좌표, 극좌표 꾸러미
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; 갇힌 프로시저
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))

  ;; 인터페이스
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 추가
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; ex 2.79
  (put 'equ? '(complex complex)
       (lambda (z1 z2) 
	 (apply-generic 'equ? z1 z2)))

  ;; ex 2.80
  (put 'zero? '(complex)
       (lambda (x)
	 (apply-generic 'zero? x)))

  'done)



;;;
(install-polar-package)
(install-rectangular-package)
(install-complex-package)

;;;---
(define (square x) (* x x))
;;;---

(define z '(complex rectangular 3 . 4))
(define z (make-complex-from-real-imag 3 4))
(magnitude z)
;; 1)
;; => (apply-generic 'magnitude '(complex rectangular 3 . 4))
;;     ->
;;      (type-tag z) -> 'complex 
;; 
;;      (get 'magnitude '(complex)) -> procedure:magnitude <-| proc
;;      (contents z) -> '(rectangular 3 . 4)
;;      (apply proc '((rectangular 3 . 4)))
;;
;; => (apply (get 'magnitude '(complex)) '((rectangular 3 . 4)))
;;           ^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;           -> (top:magnitude)
;;    --> (apply top:magnitude '((rectangular 3 . 4)))
;; => (apply magnitude '((rectangular 3 . 4)))

;; 2)
;; => (apply-generic 'magnitude '(rectangular 3 . 4))
;;     ->
;;      (type-tag z') -> 'rectangular
;;
;;      (get 'magnitude '(rectangular)) -> procedure:magnitude <-| proc
;;      (contents z') -> '(3 . 4)
;;      (apply proc '((3 . 4)))
;; => (apply (get 'magnitude '(rectangular)) '((3 . 4)))
;;           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;           -> (rectangular-package:magnitude)             ;; p236
;;    --> (apply rectangular-package:magnitude '((3 . 4)))
;;    --> (rectangular-package:magnitude '(3 . 4))
;;
;; 
;; apply-generic은 두 번 호출된다.

(real-part z) ; 3
(imag-part z) ; 4
(angle z)     ; 0.9272952180016122


;;;;;;;;;;;;

(make-complex-from-real-imag 1 1) ; '(complex rectangular 1 . 1)
(make-complex-from-mag-ang 1 1)   ; '(complex polar 1 . 1)




;;; 추가 확인

(add (make-scheme-number 1) (make-scheme-number 2))

(add (make-rational 1 2) (make-rational 2 3))

(add (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 2 3))

;;(add (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 2 3))
;; <- 구현 안되어 있음

(div (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 2 3))

;;;--------------------------< ex 2.78 >--------------------------
;;; 250
(add (make-scheme-number 1) (make-scheme-number 2)) ; '(scheme-number . 3)
(sub (make-scheme-number 1) (make-scheme-number 2)) ; '(scheme-number . -1)
(mul (make-scheme-number 1) (make-scheme-number 2)) ; '(scheme-number . 2)
(div (make-scheme-number 1) (make-scheme-number 2)) ; '(scheme-number . 1/2)

;;->
;;------------
(define (attach-tag type-tag contents)
  (if (number? contents) 
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (number? datum)
      'scheme-number
      (if (pair? datum)
	  (car datum)
	  (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (if (number? datum)
      datum
      (if (pair? datum)
	  (cdr datum)
	  (error "Bad tagged datum -- CONTENTS" datum))))
;;------------

(add 1 2) ; 3
(sub 1 2) ; -1
(mul 1 2) ; 2
(div 1 2) ; 1/2


;;;--------------------------< ex 2.79 >--------------------------
;;; 251

;;; 각 패키지 속에 
;;; equ? 함수와
;;; (put 'equ? '(타입 타입) ...) 추가함
;;; ex 2.79 로 검색

;; (define (install-general-package)
;;   ;; 인터페이스
;;   (put 'equ? '(scheme-number scheme-number)
;;        (lambda (x y) (print "scheme-number")))
;;   (put 'equ? '(rational rational) 
;;        (lambda (x y) (print "rational")))
;;   (put 'equ? '(complex complex)
;;        (lambda (x y) (print "complex")))
  
;;   'done)
;; (install-general-package)

(define (equ? x y) (apply-generic 'equ? x y))


;;;-- [[test
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
;;; 위 패키지 모두 다시 평가 후 연산 등록. 

(equ? (make-scheme-number 1) (make-scheme-number 2))

(equ? (make-scheme-number 1) (make-scheme-number 1))

(equ? 1 2)

(equ? 1 1)

(equ? (make-rational 1 2) (make-rational 2 3))

(equ? (make-rational 1 2) (make-rational 1 2))

(equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 2 3))

(equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))

(equ? (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 2 3))

(equ? (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 1 2))

;;;--]]


;;;--------------------------< ex 2.80 >--------------------------
;;; 251

;;; 각 패키지 속에 
;;; zero? 함수와
;;; (put 'zero? ('타입) ...) 추가함
;;; ex 2.80 로 검색

(define (zero? x) (apply-generic 'zero? x))

;;;-- [[test
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
;;; 위 패키지 모두 다시 평가 후 연산 등록. 

(zero? (make-scheme-number 1))

(zero? (make-scheme-number 0))

(zero? 1)

(zero? 0)

(zero? (make-rational 2 3))

(zero? (make-rational 0 2))

(zero? (make-complex-from-real-imag 2 3))

(zero? (make-complex-from-real-imag 0 0))

(zero? (make-complex-from-mag-ang 1 2))

(zero? (make-complex-from-mag-ang 0 0))

;;;--]]



;;;;================<ch 2.5.2 타입이 다른 데이터를 엮어 쓰는 방법 >=====================
;;; p251

;;; 섞붙이기(cross-type) 연산을 이용해서 타입이 다른 데이터를 엮어 쓸 수 있다.
;;; 방법 1)
;;;   - 서로 다른 타입 사이에서 일어날 수 있는 연산마다
;;;     그에 해당하는 프로시저를 하나씩 설계
;;    -> 문제점 :
;;          (1) 수많은 섞붙이기 연산 구현
;;          (2) 엮어 쓰는 데 방해.
;;  방법 2)
;;    - 타입 바꾸기


;; ;;; 방법 1의 구현
;; (define (add-complex-to-schemenum z x)
;;   (make-from-real-imag (+ (real-part z) x)
;; 		       (imag-part z)))
;; (put 'add '(complex scheme-number)
;;      (lambda (z x) (tag (add-complex-to-schemenum z x))))

;;;-------------------------
;;; 타입 바꾸기
;;; p253 

;; ;;; 방법 2의 구현
;; (define (scheme-number->complex n)
;;   (make-complex-from-real-imag (contents n) 0))
;; (put-coercion 'scheme-number 'complex scheme-number->complex)


;;; 타입 바꿈 표가 있을 때 연산 방법 :
;;; - apply-generic 에서 아래의 경우에 따라 결정
;; 1) 연산-타입 표에 있으면 그 연산 수행
;; 2) 없으면
;;    (1) 첫번째 물체를 두 번째 타입으로 바꾼다.
;;    (2) 두번째 물체를 첫 번째 타입으로 바꾼다.
;;    (3) 연산을 포기한다.
;;-> 이에 따른 apply-generic

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))   ;; 1) 경우
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(else
			 (error "No method for these types"
				(list op type-tags))))))
	      (error "No mtehod for these types"
		     (list op type-tags)))))))

;;; 위의 타입 바꾸기 문제점
;;; 두 물체 사이에서 바꾸는 방법이 없더라도
;;; 두 물체를 모두 다른 타입으로 바꾸면 연산이 가능한 경우가 있다.


;;;=================
;;; 나의 put-coercion, get-coercion 구현
;;; 타입 변환 표 구현.
;;; 타입 변환 연산 실험

(define TCT (make-hash)) ;; 타입 변환 테이블(Type Conversion Table)

(define (put-coercion type1 type2 convertor)
  (hash-set! TCT (cons type1 type2) convertor))
;;  (hash-set! GOP (list op types) behavior))
;;  (hash-set! GOP (cons 'key (cons op types)) behavior))

(define (get-coercion type1 type2)
  (let ((key (cons type1 type2)))
    (if (hash-has-key? TCT key)
	(hash-ref TCT key)
	#f)))
;;  (hash-ref GOP (list op types)))
;;  (hash-ref GOP (cons 'key (cons op types))))

;; 타입 변환 표에 변환 등록
;; ex 2.80
(define (install-type-conversion-package)
  ;; ex 2.80
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))

  ;; ex 2.81
  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)

  ;; ex 2.80
  (put-coercion 'scheme-number 'complex scheme-number->complex)

  ;; ex 2.81
  (put-coercion 'scheme-number 'scheme-number
		scheme-number->scheme-number)
  (put-coercion 'complex 'complex complex->complex)
  ;; 

  'done)


(install-type-conversion-package)

((get-coercion 'scheme-number 'complex) 1)
;; '(complex rectangular 1 . 0)


;;;-------

;; 타입 변환을 위해서 get 함수 수정
;; key가 없으면 에러를 내지 않고 #f를 리턴 하도록
(define (get op types)
  (let ((key (cons op types)))
    (if (hash-has-key? GOP key)
	(hash-ref GOP key)
	#f)))

;;; 새로운 apply-generic 실험
;;; scheme-number와 complex 간 의 더하기 연산
(add (make-scheme-number 1) (make-complex-from-real-imag 1 1))
;; '(complex rectangular 2 . 1)

;;;==================



;;;-------------------------
;;; 타입의 계층 관계
;;; p256

;;; 타입의 계층 관계 : 탑
;;; 위타입   - 복소수
;;;   ^       상수
;;;   |       유리수
;;;   v
;;; 아래타입    정수
	  
;;; 탑 구조의 장점
;;; 1) 아래 타입이 위 타입에서 정의한 연산을 모두 물려받는다.
;;;    - 복소수의 real-part -> 정수에서도 사용 가능
;;;      (정수에서 real-part : 데이터를 위 타입으로 끌어올려서 real-part 적용)
;;;
;;; 2) 데이터 물체를 단순한 표현으로 끌어내리기 쉽다
;;;    - 6+0i -> 6
;;;     복소수 -> 정수


;;;-------------------------
;;; 계층 구조가 지닌 문제점
;;; p258

;;; 보통 계층 관계가 정확하게 정의되지 않는다.
;;; - 아래 타입이 여러 개 : 끌어내리는 방법의 문제 발생
;;; - 위 타입이 여러 개  : 끌어올리는 방법의 문제 발생



;;;--------------------------< ex 2.81 >--------------------------
;;; 260


;;; 앞의 ex 2.80 install-type-conversion-package 에 내용 추가 
(install-type-conversion-package)

;;; 테스트 
((get-coercion 'scheme-number 'scheme-number) 1) ; 1

((get-coercion 'complex 'complex) (make-complex-from-real-imag 1 2))
;; '(complex rectangular 1 . 2)

(add (make-scheme-number 1) (make-scheme-number 2)) ; 3
(add (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 1))
;; '(complex rectangular 2 . 3)

;;; 잘 된다.

;;;--------------------
;;; a.

;;; 연산이 정의되어 있지 않다면?
(define (exp x y) (apply-generic 'exp x y))

;; 아래 코드를 scheme-number-package에 추가
;; ;; scheme-number 꾸러미에만 거듭제곱 프로시저가 들어있다.
;; (put 'exp '(scheme-number scheme-number)
;;      (lambda (x y) (tag (expt x y))))

(install-scheme-number-package)
;; (install-rational-package)
;; (install-rectangular-package)
;; (install-polar-package)
;; (install-complex-package)

(exp (make-scheme-number 2) (make-scheme-number 3)) ; 8 : 잘 된다.

;; 정의되지 않은 연산에 complex complex 타입 넘기면 
;;(exp (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 3 4))
;; 무한루프
;; (계속 타입만 바꾼다.)
;;
;; (1)
;->(apply-generic 'exp '(complex rectangular 2 . 3) '(complex rectangular 3 . 4))
;;
;; (2)
;-> 첫번째 if proc : false  :: 'exp '(complex complex) 연산이 없으므로
;; 
;; (3)
;-> cond ((t1->t2) 
;;         (apply-generic 'exp (t1->t2 '(complex ... 2 . 3)) '(complex ... 3 . 4))
;;  수행
;;-> 다시 apply-generic 'exp '(complex ... 3) '(complex ... 4)
;;   (1)부터 반복하는 셈.


;;;--------------------
;;; b.

;;; 타입이 같은 인자는 타입을 바꾸지 않는게 좋겠군.
;;; a)에서 보니 무한루프를 돈단 말이지..


;;;--------------------
;;; c.
;;; 두 인자가 같은 타입일 때 타입 바꾸기가 일어나지 않도록 apply-generic을 고쳐라.


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))   ;; 1) 경우
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if (eq? type1 type2)
		    (error "No method for these same types" (list op type-tags))
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else
			     (error "No method for these types"
				    (list op type-tags)))))))
		(error "No mtehod for these types"
		       (list op type-tags)))))))

;;; 테스트 
(exp (make-scheme-number 2) (make-scheme-number 3)) ; 8 : 잘 된다.

;; 정의되지 않은 연산에 complex complex 타입 넘기면 
;;(exp (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 3 4))
;; No method for these same types (exp (complex complex))
;; 정상적으로 에러 발생


;;;--------------------------< ex 2.82 >--------------------------
;;; 2

;;;--------------------------< ex 2.83 >--------------------------
;;; 2

;;;--------------------------< ex 2.84 >--------------------------
;;; 2

;;;--------------------------< ex 2.85 >--------------------------
;;; 2

;;;--------------------------< ex 2.86 >--------------------------
;;; 2


;;;;=================<ch 2.5.1 일반화된 산술 연산>=====================
;;; p245

;;;--------------------------< ex 2.87 >--------------------------
;;; 2


;;;--------------------------< ex 2.88 >--------------------------
;;; 2


;;;--------------------------< ex 2.89 >--------------------------
;;; 2



;;;--------------------------< ex 2.90 >--------------------------
;;; 2


;;;--------------------------< ex 2.91 >--------------------------
;;; 2


;;;--------------------------< ex 2.92 >--------------------------
;;; 2


;;;--------------------------< ex 2.93 >--------------------------
;;; 2


;;;--------------------------< ex 2.94 >--------------------------
;;; 2


;;;--------------------------< ex 2.95 >--------------------------
;;; 2

;;;--------------------------< ex 2.96 >--------------------------
;;; 2

;;;--------------------------< ex 2.97 >--------------------------
;;; 2

