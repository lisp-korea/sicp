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
  (* (magnitude z) (cons (angle z))))

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


;;;;=================<ch 2.4.3 데이터 중심 프로그래밍과 덧붙임 성질>=====================
;;; p233



;;;--------------------------< ex 2.73 >--------------------------
;;; 239,40

;;;--------------------------< ex 2.74 >--------------------------
;;; 240,1


;;;--------------------------------
;;; 메시지 패싱

;;;--------------------------< ex 2.75 >--------------------------
;;; 243

;;;--------------------------< ex 2.76 >--------------------------
;;; 243

