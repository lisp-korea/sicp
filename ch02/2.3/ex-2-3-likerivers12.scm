;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ch 2 데이터를 요약해서 표현력을 끌어올리는 방법
;;; Ch 2.3 글자 데이터

;;;;=================<ch 2.3.1 따옴표 연산>=====================
;;; p184

;;; p186
(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))

(memq 'apple '(x (apple sauce) y apple pear))

(memq 'apple '(x y apple))


;;;--------------------------< ex 2.53 >--------------------------
;;; p187

(list 'a 'b 'c) ; '(a b c)

(list (list 'george)) ; '((george))

(cdr '((x1 x2) (y1 y2))) ; '((y1 y2))

(cadr '((x1 x2) (y1 y2))) ; '(y1 y2)

(pair? (car '(a short list))) ; #f

(memq 'red '((red shoes (blue socks)))) ; #f

(memq 'red '(red shoes blue socks)) ; '(red shoes blue socks)


;;;--------------------------< ex 2.54 >--------------------------
;;; p187

(define (my-equal? a b)
  (cond ((eq? a b) #t)
	((eq? (car a) (car b)) (my-equal? (cdr a) (cdr b)))
	(else #f)))

(my-equal? '(this is a list) '(this is a list)) ; #t

(my-equal? '(this is a list) '(this (is a) list)) ; #f


;;;--------------------------< ex 2.55 >--------------------------
;;; p188

(car ''abracadabra) ; 'quote
;; = (car (quote (quote abracadabra)))
;;               ^^^^^^^^^^^^^^^^^^^
;;                                ^
;;             최초의 quote에 의해서 | 이 만큼이 car의 인자로 들어가는 데이터이다

(quote abracadabra) ; 'abracadabra


;;;;=================<ch 2.3.2 연습 : 글자 식의 미분>=====================
;;; p188


;;;--------------------------------
;;; 요약된 데이터로 미분하는 프로그램 짜기
;;; p189

;;;;;;;;;;;;;;;
;; (variable? e)
;; (same-variable? v1 v2)

;; (sum? e)
;; (addend e)
;; (augend e)
;; (make-sum a1 a2)

;; (product? e)
;; (multiplier e)
;; (multiplicand e)
;; (make-product m1 m2)
;;;;;;;;;;;;;;;;

;; 1) dc/dx = 0
;; 2) dx/dx = 1
;; 3) d(u+v)/dx = du/dx + dv/dx
;; 4) d(uv)/dx = u(dv/dx) + (du/dx)v

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;; 	((variable? exp)
;; 	 (if (same-variable? exp var) 1 0))
;; 	((sum? exp)
;; 	 (make-sum (deriv (addend exp) var)
;; 		   (deriv (augend exp) var)))
;; 	((product? exp)
;; 	 (make-sum
;; 	  (make-product (multiplier exp)
;; 			(deriv (multiplicand exp) var))
;; 	  (make-product (deriv (multiplier exp) var)
;; 			(multiplicand exp))))
;; 	(else
;; 	 (error "unknown expression type -- DERIV" exp))))

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
;;<- 더하기에 대해서 augend를 앞에 , addend를 뒤에 써야 정의에 맞는 순서가 나온다.
;;<- 곱하기에 대해서 multiplicand를 앞에 multiplier를 뒤에 써야 정의에 맞는 순서가 나온다.


;;;--------------------------------
;;; 대수식 표현하기
;;; p191

;;;* 
(define (variable? x) (symbol? x))

;;;*
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;;*
(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

;;;*
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;;;*
(define (augend s) (cadr s))

;;;*
(define (addend s) (caddr s))

;;;*
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;;;*
(define (multiplicand p) (cadr p))

;;;*
(define (multiplier p) (caddr p))


;;;
(deriv '(+ x 3) 'x)  ; '(+ 1 0)

(deriv '(* x y) 'x)  ; '(+ (* x 0) (* 1 y))

(deriv '(* (* x y) (+ x 3)) 'x)
;; '(+ (* (* x y) (+ 1 0)) 
;;     (* (+ (* x 0) (* 1 y)) 
;;        (+ x 3)))



;;; 복잡한 수식을 고쳐쓰기 위해
;;; make-sum, make-product 다시 정의

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

(deriv '(+ x 3) 'x) ; 1

(deriv '(* x y) 'x) ; 'y

(deriv '(* (* x y) (+ x 3)) 'x) ; '(+ (* x y) (* y (+ x 3)))




;;;--------------------------< ex 2.56 >--------------------------
;;; p194

;;; d(u^n)/dx = nu^(n-1)(du/dx)


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
	((exponentiation? exp)
	 (make-product (exponent exp)
		       (make-product 
			(make-exponentiation (base exp)
					     (- (exponent exp) 1))
			(deriv (base exp) var))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(exponentiation? '(** 2 3)) ; #t

(define (make-exponentiation b e)
  (list '** b e))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(deriv '(** x 3) 'x) ; '(* 3 (** x 2))

(deriv '(* (** x 3) (+ (* 2 x) 3)) 'x)
;; '(+ (* (** x 3) 2) 
;;     (* (* 3 (** x 2)) 
;;        (+ (* 2 x) 3)))


  


;;;--------------------------< ex 2.57 >--------------------------
;;; p195

;;; 여러 마디로 이루어진 덧셈,곱셈 식을 처리할 수 있도록


;;;*
(define (addend s) 
  (cond ((null? (cdddr s)) (caddr s))   ; '(+ 1 2) -> 2
	(else (cons '+ (cddr s)))))     ; '(+ 1 2 3) -> (+ 2 3)

(addend '(+ 1 2))   ; 2
(addend '(+ 1 2 3)) ; '(+ 2 3)


;;;*
(define (multiplier p)
  (cond ((null? (cdddr p)) (caddr p))   ; '(* 1 2) -> 2
	(else (cons '* (cddr p)))))     ; '(* 1 2 3) -> (* 2 3)

(deriv '(* x y (+ x 3)) 'x)
;; '(+ (* x y) (* y (+ x 3)))


;;;--------------------------< ex 2.58 >--------------------------
;;; p195

;; (x + (3 * (x + (y + 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a)

;;;*
(define (make-sum a1 a2) (list a1 '+ a2))

(define (make-product m1 m2) (list m1 '* m2))

;;;*
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

;;;*
(define (augend s) (car s))

;;;* 그대로
(define (addend s) (caddr s))

;;;*
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

;;;*
(define (multiplicand p) (car p))

;;;* 그대로
(define (multiplier p) (caddr p))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
;; '(1 + ((3 * (1 + (0 + 0))) + (0 * (x + (y + 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; b)
;; 가능하겠지만, 무지 어려워보인다.




;;;;=================<ch 2.3.3 연습 : 집합을 나타내는 방법>=====================
;;; p196


;;;--------------------------------
;;; 차례 없는 리스트로 표현한 집합
;;; p197


;;;--------------------------< ex 2.59 >--------------------------
;;; p1
