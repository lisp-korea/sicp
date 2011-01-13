;; -*- coding: utf-8 -*-

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.4.1 복소수표현
(make-from-real-imag (real-part z) (imag-part z))
(make-from-mag-ang (magnitude z) (angle z))

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

;; Ben의 표현방식을 정의하는 짜뭊추개와 고르개 (직각좌표방식)
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;; Alissa의 표현방식을 정의하는 짜뭊추개와 고르개 (극좌표방식)
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.4.2 타입을 표시한 데이터

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
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Ben의 표현방식을 정의하는 짜뭊추개와 고르개 (직각좌표방식)
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z)) (square (imag-part z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular
              (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;; Alissa의 표현방식을 정의하는 짜뭊추개와 고르개 (극좌표방식)
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
(define (make-from-mag-ang r a)
  (attach-tag 'polar
              (cons r a)))

;; 일반화된 고르개 연산
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
         (imag-part-rectangular (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-polar (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))


(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.4.3 데이터 중심 프로그래밍과 덧붙임 성질

(define (install-rectangular-package)
  ;; 갇힌 프로시저
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; 이꾸러미의 인터페이스
  (define (tag x) (attatch-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; 갇힌 프로시저
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; 이꾸러미의 인터페이스
  (define (tag x) (attatch-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        <추가된 규칙들은 여기에 붙여진다.>
        (else (error "unknown expresssion type -- DERIVE" exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.73.a.
;; 위에서 한일이 무엇인지 설명해 보라. number?나 variable?같은 술어 프로시저를 모조리
;; 데이터 중심방식으로 다루지 못하는 까닭은 무엇인가?
;;
;; 0) exp의 operator에 따라 적절한 deriv를 리턴한다.
;; 1) number? variable?다음에 operator가 없다. 모델링의 요구사항에 맞지 않는다.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.73.b. 덧셈과 곱셈식을 미분하는 프로시절들을 짜라. 그런다음, 그
;; 프로시저들을 위 프로그램에서 쓰는 표에 집어넣는데 필요한 코드를 덧붙여라.
;; 

;; 해쉬테이블관련코드
(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (install-deriv-package)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product))

(install-deriv-package)
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.73.c. 지수식을 미분하는 것과 같이, 새로 모태고 싶은 규칙을 하나
;; 골라 여기서 만든 데이터 중심 시스템에 집어넣어라.
;; 

(define (install-deriv-exponentiation-package)
  (define (base s) (car s))
  (define (exponent s) (cadr s))
  (define (make-exponentiation b e)
    (cond ((=number? b 0) 0)
          ((=number? e 1) b)
          (else (list '** b e))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (deriv-exponentiation exp var)
    (make-product
     (exponent exp)
     (make-product
      (make-exponentiation (base exp) (- (exponent exp) 1))
      (deriv (base exp) var))))
  (put 'deriv '** deriv-exponentiation))

(install-deriv-exponentiation-package)
(deriv '(** x 3) 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.73.d. 이런 단순한 대수 처리방식에서는, 식에 붙은 연산자가 그
;; 식의 타입을 나타낸다. 이때 '연산자와 타입'이 아니라 '타입과
;; 연산자'로 프로시저 인덱스가 붙어 있다면, deriv속에서 알맞은
;; 프로시저를 꺼내 쓰는 코드가 다음과 같이 바뀐다.
;; ((get (operator exp) 'deriv) (operands exp) var)
;; 이경우, 이분 시스템에는 어떤 변화가 필요한가?
;;
;; put의 인자 순서를 get과 같이 (operator exp) 'deriv순서로 변경해준다.


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.74
;; 부서파일은 부서이름+이것저것 의 형식이라고 가정한다.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.74.a
;; rec_part 부서이름
;; rec_name 직원이름
(define (get-record rec_part rec_name)
  ((get 'get-record rec_part) rec_name))
(put 'get-record rec_part get-record-custom)
;; 각 부서 파일의 레코드는 get-record-custom이 제대로 동작 하도록 자치적이면 된다.
;; 이문제는 부서이름과 직원이름을 꼭 알아야 풀 수 있다.


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.74.b
;; rec_part 부서이름
;; rec_name 직원이름
(define (get-salary rec_part rec_name)
  ((get 'get-salary rec_part) rec_name))
(put 'get-salary rec_part get-salary-custom)
;; 각 부서 파일의 레코드는 get-salary-custom이 제대로 동작 하도록 자치적이면 된다.
;; 이문제는 부서이름과 직원이름을 꼭 알아야 풀 수 있다.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.74.c
;; 문제를 간단히 하기위해 부서이름은 부서파일의 앞에 위치한다.

(define (find-employee-record rec_name l_rec_files)
  (define (get-part file) (car file))
  (map (lambda (file) get-record (get-part file) rec_name)
       l_rec_files))
  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.74.d
;;
;; ex.2.74.a, ex.2.74.b와 같이 자치적인 get-record, get-salary를 제작하여
;; 해시맵에 put합니다.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 메시지 패싱
(define (square x) (* x x))
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
(define (apply-generic op arg) (arg op))
(apply-generic 'real-part (make-from-real-imag 1 2))
(apply-generic 'imag-part (make-from-real-imag 1 2))
(apply-generic 'magnitude (make-from-real-imag 1 2))
(apply-generic 'angle (make-from-real-imag 1 2))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.75
;;
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
(define (apply-generic op arg) (arg op))
(apply-generic 'real-part (make-from-mag-ang 1 2))
(apply-generic 'imag-part (make-from-mag-ang 1 2))
(apply-generic 'magnitude (make-from-mag-ang 1 2))
(apply-generic 'angle (make-from-mag-ang 1 2))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.76 일화된 연산을 갖춘 커다란 시스템이 오랜 시간에 걸쳐 진화를
;; 거듭하면, 그 에 따라 새로운 데이터 타입이나 연산이 필요 할 수 있다.
;; 지금까지 살펴본 세방식, 즉 일을 집적 나눠맡기는 방식(explicit
;; dispatch), 데이터 중식방식, 메시지패싱방식에 따라 새로운 연산을
;; 집어넣거나 새로운 타입을 보탠다고 하면, 시스템에 어떤 변화가
;; 일어나는가? 새로운 데이터 타입을 집어 넣어야 할일이 많다면, 어떤
;; 방식으로 시스템을 짜맞추는게 가장 좋은가? 새로운 연산을 덧붙이는
;; 경우가 많을때에는 어떤 방식이 가장 적당한가?


;; 0) i)  일을직접나눠맡기는방식
;;        - 새로운 이름의 프러시저가 늘어난다.
;;        - 사용시 새로운 이름의 프러시저를 직접 사용한다.
;;    ii) 데이터중심방식
;;        - 새로운 이름의 프러시저가 늘어난다. 이 프러시저를 해시테이블에 등록한다.
;;        - 사용시 이전과 같은 이름의 프러시저를 사용한다. 데이터에 따라 등록된 프러시저가 사용된다.
;;    iii)메시지패싱방식
;;        - 기존의 프러시저에 
;;
;; 1) 새로운 데이터 타입을 집어 넣어야 할일이 많다면 데이터중심방식이 좋다.
;; 2) 새로운 연산을 덧붙이는 일이 많다면 메시지패싱방식이 좋다.