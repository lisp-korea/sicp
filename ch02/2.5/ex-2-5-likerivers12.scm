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
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;; p248
;;; complex
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
  'done)

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

;;;--------------------------< ex 2.78 >--------------------------
;;; 2

;;;--------------------------< ex 2.79 >--------------------------
;;; 2

;;;--------------------------< ex 2.80 >--------------------------
;;; 2


;;;;=================<ch 2.5.2 타입이 다른 데이터를 엮어 쓰는 방법 >=====================
;;; p251

;;;--------------------------< ex 2.81 >--------------------------
;;; 2

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

