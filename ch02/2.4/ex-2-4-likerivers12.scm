;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 2 데이터를 요약해서 표현력을 끌어올리는 방법
;;; Ch 2.4. 요약된 데이터의 표현 방식이 여러 가지일 때

;;;;=================<ch 2.4.1 복소수 표현>=====================
;;; p223

;;; p225
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


;;;-----
(define (square x) (* x x))
;;;-----

;;; 직각 좌표 방식
(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;;   1 + 1i
;; = sqrt(2)/(pi/4)

(define z1 (make-from-real-imag 1 1))
z1   ; '(1 . 1)

(define z2 (make-from-mag-ang (sqrt 2) (/ 3.14159 4)))
z2   ; '(1.0000006633972283 . 0.9999993366023316)

(real-part z1) ; 1
(real-part z2) ; 1.0000006633972283

(imag-part z1) ; 1
(imag-part z2) ; 0.9999993366023316

(magnitude z1) ; 1.4142135623730951
(magnitude z2) ; 1.4142135623730951

(angle z1) ; 0.7853981633974483
(angle z2) ; 0.7853975


;;; 극좌표 방식
(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (mag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
	(atan y x)))

(define (make-from-mag-ang r a)
  (cons r a))

(define z1 (make-from-real-imag 1 1))
z1   ; '(1.4142135623730951 . 0.7853981633974483)

(define z2 (make-from-mag-ang (sqrt 2) (/ 3.14159 4)))
z2   ; '(1.4142135623730951 . 0.7853975)

(real-part z1) ; 1.4142135623730951
(real-part z2) ; 1.4142135623730951

(imag-part z1) ; 0.7853981633974483
(imag-part z2) ; 0.7853975

(magnitude z1) ; 1.4142135623730951
(magnitude z2) ; 1.4142135623730951

(angle z1) ; 0.7853981633974483
(angle z2) ; 0.7853975

;;;;=================<ch 2.4.2 타입을 표시한 데이터>=====================
;;; p228

;;; 데이터 요약 - '판단을 한껏 미루자는 원칙'

;;; 여러 표현 방식을 모두 받아들이도록 설계 
;;; - 타입을 표시

;;; p229
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


;; 타입 확인
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


;; 직각 좌표계 수정 - Ben
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
	      (cons (* r (cos a)) (* r (sin a)))))

;; 극 좌표계 수정 - Alyssa
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


;;; 일반화된 고르개 연산
;;; - 물체에 붙은 표시를 살펴보고 알맞은 프로시저를 불러쓴다.
;;; p231

(define (real-part z)
  (cond ((rectangular? z)
	 (real-part-rectangular (contents z)))
	((polar? z)
	 (real-part-polar (contents z)))
	(else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
	 (imag-part-rectangular (contents z)))
	((polar? z)
	 (imag-part-polar (contents z)))
	(else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
	 (magnitude-rectangular (contents z)))
	((polar? z)
	 (magnitude-polar (contents z)))
	(else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
	 (angle-rectangular (contents z)))
	((polar? z)
	 (angle-polar (contents z)))
	(else (error "Unknown type -- ANGLE" z))))

;; ->
;; 복소수의 산술 연산에서는 add-complex, ,,, , div-complext를 그대로 가져다 쓸 수 있다.
;; p232의 add-complex와 p225의 add-complex 비교 - 같다


;;; 복소수를 만들 때 직각좌표(Ben), 극좌표(Alyssa) 선택
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))


;;; 앞선 복소수 시스템의 구성 요소
;; 1) 복소수 산술 연산
;; 2) 극좌표 구현
;; 3) 직각 좌표 구현


;;; 고르개 연산이 데이터 타입을 가리지 않고 돌아가는 까닭은
;;; 데이터 물체마다 타입을 나타내는 표시가 붙어있기 때문임.

;;; 층과 층 사이에서 데이터를 주고 받으며 표시를 붙였다 뗐다 하는 기법 ***


;;;;=================<ch 2.4.3 데이터 중심 프로그래밍과 덧붙임 성질>====================
;;; p233

;; 데이터 타입에 따라 나누어 맡기기
;; -> 약점
;; 1) 일반화된 인터페이스 프로시저를 만들고자할 때 모든 표현 방식을 미리 알아야 한다.

;;; 데이터 중심 프로그래밍
;;; - 서로 다른 여러 데이터 타입 사이에 공통된 연산을 다루는 것 
;;;   -> 연산과 타입을 두 축으로 하는 이차원 표를 다루는 것과 같다.

;; 일단, put, get 이 있다고 가정.

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
  'done)


;;
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

;;
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;;;-----------------------------------------------
;;; put, get 구현
(define GOP (make-hash))

(define (put op types behavior)
  (hash-set! GOP (cons op types) behavior))
;;  (hash-set! GOP (cons 'key (cons op types)) behavior))

(define (get op types)
  (hash-ref GOP (cons op types)))
;;  (hash-ref GOP (cons 'key (cons op types))))
;;;---

(install-rectangular-package)
(install-polar-package)

(define z11 (make-from-real-imag 1 1))
(define z12 (make-from-real-imag 2 2))

(define z21 (make-from-mag-ang 1.414 (/ 3.14159 4)))
(define z22 (make-from-mag-ang 2.828 (/ 3.14159 4)))

(real-part z11)
(imag-part z12)
(magnitude z21)
(angle z22)
;;;-----------------------------------------------



;;;--------------------------< ex 2.73 >--------------------------
;;; 239,40

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (augend exp) var) 
		   (deriv (addend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplicand exp)
			(deriv (multiplier exp) var))
	  (make-product (deriv (multiplicand exp) var)
			(multiplier exp))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

;;-> 데이터 중심 방식으로 

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	                                   var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; a.
;; 데이터 중심으로 바꾸기 위해서 exp의 연산 종류에 따라서 deriv를 수행하는 연산을 가져옴.
;; 가져온 프로시저에 exp의 값과 미분 변수를 적용하여 실제 미분을 수행
;; .술어 프로시저를 데이터 중심으로 바꾸지 못하는 까닭?
;;  - 물체의 타입 정보가 없기 때문.


;; b,c
(define (install-deriv-package)
  ;; from p191
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
;;  (define (make-sum a1 a2) (list '+ a1 a2))
;;  (define (make-product m1 m2) (list '* m1 m2))
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (augend s) (car s))      ;<- 수정
  (define (addend s) (cadr s))     ;<- 수정
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (multiplicand p) (car p))   ;<- 수정
  (define (multiplier p) (cadr p))    ;<- 수정 (operator를 빼고 들어오기 때문)

  (define (base e)
    (car e))                ;<- 수정
  (define (exponent e)
    (cadr e))               ;<- 수정
  (define (make-exponentiation b e)
    (list '** b e))

  ;; from p190..
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2)) (+ a1 a2))
	  (else (list '+ a1 a2))))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	  ((=number? m1 1) m2)
	  ((=number? m2 1) m1)
	  ((and (number? m1) (number? m2)) (* m1 m2))
	  (else (list '* m1 m2))))

  (define (deriv-sum exp var)
    (make-sum (deriv (augend exp) var) 
	      (deriv (addend exp) var)))
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplicand exp)
		   (deriv (multiplier exp) var))
     (make-product (deriv (multiplicand exp) var)
		   (multiplier exp))))
  (define (deriv-exp exp var)
    (make-product (exponent exp)
		  (make-product 
		   (make-exponentiation (base exp)
					(- (exponent exp) 1))
		   (deriv (base exp) var))))
    
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exp)
  'done)


(install-deriv-package)


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(** x 2) 'x)

;; d.
;; 변화가 없다.

;;;--------------------------< ex 2.74 >--------------------------
;;; 240,1

;;; a. 
;;; 부서의 종류를 알 수 있어야 하고,
;;; 해당 부서에서 지정된 직원 레코드를 뽑아내는 연산이 무엇인지 알아야 한다.

;;; b.
;;; 


;;; c.
;;;

;;; d.
;;;

;;;--------------------------------
;;; 메시지 패싱
;;; p242

;;; 똑똑한 데이터 물체가 연산 이름에 따라 일을 나누어 처리한다고 보는 방식

;;; 데이터 물체를 프로시저로 나타내고,
;;; 그 프로시저가 연산 이름을 인자로 받아서 그 이름이 가리키는 연산을 처리하도록
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
	  ((eq? op 'imag-part) y)
	  ((eq? op 'magnitude)
	   (sqrt (+ (square x) (square y))))
	  ((eq? op 'angle) (atan y x))
	  (else
	   (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)


;;;--------------------------< ex 2.75 >--------------------------
;;; 243

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* (magnitude z) (cons (angle z))))
	  ((eq? op 'imag-part) (* (magnitude z) (sin (angle z))))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else
	   (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)


;;;--------------------------< ex 2.76 >--------------------------
;;; 243

