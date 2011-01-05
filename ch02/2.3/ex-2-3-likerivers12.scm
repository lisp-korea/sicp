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

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(element-of-set? 1 '(2 3 1)) ; #t
(element-of-set? 4 '(2 3 1)) ; #f

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(adjoin-set 1 '(2 3 1)) ; '(2 3 1)
(adjoin-set 4 '(2 3 1)) ; '(4 2 3 1)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(intersection-set '(1 2) '(2 3 1)) ; '(1 2)
(intersection-set '(4) '(2 3 1))   ; '()


;;;--------------------------< ex 2.59 >--------------------------
;;; p198

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((not (element-of-set? (car set1) set2))
	 (cons (car set1)
	       (union-set (cdr set1) set2)))
	(else (union-set (cdr set1) set2))))

(union-set '(1 2) '(2 3 1)) ; '(2 3 1)
(union-set '(4 2) '(2 3 1)) ; '(4 2 3 1)

;;;--------------------------< ex 2.60 >--------------------------
;;; p198

(define (element-of-red-set? x set)
  (element-of-set? x set))

(element-of-red-set? 1 '(2 3 2 1 3 1)) ; #t
(element-of-red-set? 4 '(2 3 2 1 3 1)) ; #f

(define (adjoin-red-set x set2)
  (cons x set2))

(adjoin-red-set 1 '(2 3 2 1 3 1)) ; '(1 2 3 2 1 3 1)
(adjoin-red-set 4 '(2 3 2 1 3 1)) ; '(4 2 3 2 1 3 1)

(define (intersection-red-set set1 set2)
  (intersection-set set1 set2))

(intersection-red-set '(1 2) '(2 3 2 1 3 1)) ; '(1 2)
(intersection-red-set '(4) '(2 3 2 1 3 1))   ; '()

(define (union-red-set set1 set2)
  (append set1 set2))

(union-red-set '(1 2) '(2 3 2 1 3 1)) ; '(1 2 2 3 2 1 3 1)
(union-red-set '(4 2) '(2 3 2 1 3 1)) ; '(4 2 2 3 2 1 3 1)

;; adjoin-set과 union-set에서 합칠 원소가 
;; 기존의 집합에 이미 있는 원소인지 확인할 필요가 없으므로
;; 그냥 리스트에 덧붙여 주면 된다.
;; 그러므로 adjoin-set은 O(1) 의 시간 복잡도를 가지고,
;; union-set은 첫번째 집합이 n개의 원소를 가질 때 O(n)의 시간 복잡도를 가진다.

;; element-of-set과 intersection-set은 기존의 방법을 그대로 쓰면 되므로
;; 기존과 같은 시간복잡도를 가진다.
;; 그러나 adjoin-set과 union-set에서 중복된 원소가 발생되면 
;; 기존 방법보다 집합의 길이가 더 커질 수 있다.
;; 이러한 경우 기존방법보다 시간이 더 많이 걸릴 수 있다.

;; 집합의 원소가 될 수 있는 후보가 아주 많아서 중복된 원소가 잘 나타나지 않는 문제 영역,
;; 데이터를 추가하고 합하는 상황이 많이 발생되는 문제 영역.
;; 어떤 사건이 발생한 날짜의 집합?


;;;--------------------------------
;;; 차례 매긴 리스트로 표현된 집합
;;; p198

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(element-of-set? 2 '(1 2 4 5)) ; #t

(element-of-set? 3 '(1 2 4 5)) ; #f

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-set (cdr set1)
				       (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set set1 (cdr set2)))))))

(intersection-set '(1 3) '(1 2 3 4 5)) ; '(1 3)

;;;--------------------------< ex 2.61 >--------------------------
;;; p201

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< x (car set))
	 (cons x set))
	((= x (car set)) set)	 
	(else 
	 (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 1 '(1 2 3)) ; '(1 2 3)
(adjoin-set 4 '(1 2 3)) ; '(1 2 3 4)


;;;--------------------------< ex 2.62 >--------------------------
;;; p201

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else 
	 (let ((x1 (car set1))
	       (x2 (car set2)))
	   (cond ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
		 ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
		 ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2)))))))))

(union-set '(1 2) '(1 2 3)) ; '(1 2 3)
(union-set '(2 4) '(1 2 3)) ; '(1 2 3 4)
(union-set '(1 2 3) '(3 5)) ; '(1 2 3 5)



;;;--------------------------------
;;; 두 갈래 나무로 표현한 집합
;;; p201

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))


;;           7(t6)
;;           ^
;;         /   \ 
;;    (t5)3     9(t4)
;;       /  \      \
;;  (t3)1    5(t2)  11(t1)

(define t1 (make-tree 11 '() '()))
(define t2 (make-tree 5 '() '()))
(define t3 (make-tree 1 '() '()))
(define t4 (make-tree 9 '() t1))
(define t5 (make-tree 3 t3 t2))
(define t6 (make-tree 7 t5 t4))
;; '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))

(element-of-set? 1 t6) ; #t
(element-of-set? 2 t6) ; #f
(element-of-set? 10 t6) ; #f
(element-of-set? 11 t6) ; #t


;; (define l1 (make-tree 2 '() '())) ;l1
;; (define r1 (make-tree 8 '() '())) ;r1
;; (define root (make-tree 5 l1 r1)) ;root -> ; '(5 (2 '() '()) (8 '() '()))

;; (element-of-set? 1 root) ;#f
;; (element-of-set? 2 root) ;#t


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

(adjoin-set 10 t6) ; '(7 (3 (1 () ()) (5 () ())) (9 () (11 (10 () ()) ())))
(adjoin-set 11 t6) ; '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))


;;;--------------------------< ex 2.63 >--------------------------
;;; p205

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(tree->list-1 t6) ; '(1 3 5 7 9 11)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

(tree->list-2 t6) ; '(1 3 5 7 9 11)

;;------------------------------------------
;;           3(t6)
;;           ^
;;         /   \ 
;;    (t5)1     7(t4)
;;            /   \
;;           5(t3)  9(t2)
;;                   \
;;                    11(t1)

(define t1 (make-tree 11 '() '())) ;t1
(define t2 (make-tree 9 '() t1)) ; t2
(define t3 (make-tree 5 '() '()))
(define t4 (make-tree 7 t3 t2))
(define t5 (make-tree 1 '() '()))
(define t6 (make-tree 3 t5 t4)) ;t6
;; '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))

(tree->list-1 t6) ;'(1 3 5 7 9 11)
(tree->list-2 t6) ;'(1 3 5 7 9 11)

;;------------------------------------------
;;           5(t6)
;;           ^
;;         /   \ 
;;    (t5)3     9(t4)
;;      /      /  \
;;    1(t3)  7(t2) 11(t1)

(define t1 (make-tree 11 '() '())) ;t1
(define t2 (make-tree 7 '() '())) ;t2
(define t3 (make-tree 1 '() '()))
(define t4 (make-tree 9 t2 t1))
(define t5 (make-tree 3 t3 '()))
(define t6 (make-tree 5 t5 t4)) ;t6
;; '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))

(tree->list-1 t6) ;'(1 3 5 7 9 11)
(tree->list-2 t6) ;'(1 3 5 7 9 11)

;; a) 모두 같은 결과를 내놓는다.

;; b) 



;;;--------------------------< ex 2.64 >--------------------------
;;; p206

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

(list->tree '(1 3 5 7 9 11)) ; '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

(partial-tree '(1 3 5 7 9 11) 6)
;; ->
  (partial-tree '(1 3 5 7 9 11) 2) ; 첫번째 left-result : (partial-tree elts 2)

;; a) : list의 
;;  - 첫 번째 원소를 left tree, 
;;  - 두 번째 원소를 entry, 
;;  - 세 번째 원소를 right tree
;; 형태로 하나의 리스트로 묶어서
;; 상위 리스트의 한 원소로 할당한다.
;; 나머지 원소들을 그 뒤에 붙인다.
;; (a b c d ...) -> ( ( b (t a) (t c) ) d ...)
;;  ^^^^^             ^^^^^^^^^^^^^^^^^

;; b) : 


;;;--------------------------< ex 2.65 >--------------------------
;;; p207

(define (union-set-bt bt-set1 bt-set2)
  (let ((lst1 (tree->list-2 bt-set1))
	(lst2 (tree->list-2 bt-set2)))
    (list->tree (union-set lst1 lst2))))

(union-set-bt (list->tree '(1 2 3)) (list->tree '(3 4 5)))
;; '(3 (1 () (2 () ())) (4 () (5 () ())))

(define (intersection-set-bt bt-set1 bt-set2)
  (let ((lst1 (tree->list-2 bt-set1))
	(lst2 (tree->list-2 bt-set2)))
    (list->tree (intersection-set lst1 lst2))))

(intersection-set-bt (list->tree '(1 2 3 4)) (list->tree '(3 4 5 6)))
;; '(3 () (4 () ()))

;;;--------------------------------
;;; 집합에서 정보 찾아내기
;;; p207

;; (define (lookup given-key set-of-records)
;;   (cond ((null? set-of-records) false)
;; 	((equal? given-key (key (car set-of-records)))
;; 	 (car set-of-records))
;; 	(else (lookup given-key (cdr set-of-records)))))


;;;--------------------------< ex 2.66 >--------------------------
;;; p209

(define (lookup-bt given-key tree)
  (cond ((null? tree) false)
	((< given-key (tree-key tree))
	 (lookup-bt given-key (left-branch tree)))
	((> given-key (tree-key tree))
	 (lookup-bt given-key (right-branch tree)))
	((equal? given-key (tree-key tree))
	 (tree-val tree))))  

;;-------------------
;; 키와 값이 같은 tree
(define (tree-key tree)
  (entry tree))

(define (tree-val tree)
  (entry tree))
;;-------------------

;;-----------------------
;;           7(t6)
;;           ^
;;         /   \ 
;;    (t5)3     9(t4)
;;       /  \      \
;;  (t3)1    5(t2)  11(t1)

(define t1 (make-tree 11 '() '()))
(define t2 (make-tree 5 '() '()))
(define t3 (make-tree 1 '() '()))
(define t4 (make-tree 9 '() t1))
(define t5 (make-tree 3 t3 t2))
(define t6 (make-tree 7 t5 t4))
;; '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))
;;-----------------------

(lookup-bt 3 t6)



;;;;=================<ch 2.3.4 연습 : 허프만 인코딩 나무>=====================
;;; p209


;;;--------------------------------
;;; 허프만 나무 만들기
;;; p213
;;; 1) 나뭇잎 집합 - {(글자, 빈도수) , ... }
;;; 2) 가장 가벼운 잎 둘을 찾아서,
;;;     두 잎을 오른쪽 가지와 왼쪽 가지로 삼고
;;;     마디로 묶어낸다. 
;;;     마디의 무게는 두 잎의 무게의 합.
;;; 3) 되풀이

;;;--------------------------------
;;; 허프만 나무를 표현하는 방법
;;; p214

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;;; 두 집합을 합할 때
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;;--------------------------------
;;; 디코딩 프로시저
;;; p216

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

;;;--------------------------------
;;; 무게가 있는 원소들의 집합
;;; p217

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)   ; 글자
			       (cadr pair)) ; 빈도
		    (make-leaf-set (cdr pairs))))))


;;;--------------------------< ex 2.67 >--------------------------
;;; p218

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
	  
(decode sample-message sample-tree)
;; -> '(A D A B B C A)

;;;--------------------------< ex 2.68 >--------------------------
;;; p218

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (null? tree) 
      (error "not contained symbol") ;#f
      (if (leaf? tree)
	 (let ((sym1 (symbol-leaf tree)))
	   (if (eq? symbol sym1)
	       '()
	       (error "not contained symbol"))) ;#f))
	 (let ((lt (left-branch tree))
	       (rt (right-branch tree)))
	   (if (element-of-set? symbol (symbols lt))
	       (cons 0 (encode-symbol symbol lt))
	       (cons 1 (encode-symbol symbol rt)))))))
	   

;;;---
;;; 차례없는 집합의 원소 확인
;;; p197
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))
;;;---

(encode-symbol 'A sample-tree) ; '(0)
(encode-symbol 'B sample-tree) ; '(1 0)
(encode-symbol 'I sample-tree) ; error

(encode '(A D A B B C A) sample-tree)
;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
;; sample message와 같은 결과가 나온다.



;;;--------------------------< ex 2.69 >--------------------------
;;; p218,9

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define pairs '((A 4) (B 2) (C 1) (D 1)))

;; pairs가 빈도의 내림차순으로 입력되면
;; leaf-set은 빈도의 오름차순으로 구성된다.
(make-leaf-set pairs) ; '((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))

;; leaf-set은 빈도의 오름차순이라고 가정한다.
(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
	((null? (cdr leaf-set)) '())
	(else
	 (let ((t1 (car leaf-set))
	       (t2 (cadr leaf-set))
	       (rest (cddr leaf-set)))
	   (let ((m (merge-two t1 t2)))
	     (if (null? rest)
		 m
		 (successive-merge (adjoin-set m rest))))))))
	  
(define (merge-two t1 t2)
  (make-code-tree t1 t2))


(generate-huffman-tree pairs)
;; '((leaf A 4)
;;   ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)
;;   (A B D C)
;;   8)

(encode '(A D A B B C A) (generate-huffman-tree pairs))
;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)

(decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) (generate-huffman-tree pairs))
;; '(A D A B B C A)


;;;--------------------------< ex 2.70 >--------------------------
;;; p219,20

(define pairs '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (BOOM 1) (WAH 1)))

(define huff-tree (generate-huffman-tree pairs))

(encode '(GET A JOB) huff-tree) ; '(1 1 1 1 1 1 1 0 0 1 1 1 1 0)
(encode '(SHA NA NA NA NA NA NA NA NA) huff-tree) ;'(1 1 1 0 0 0 0 0 0 0 0 0)
(encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) huff-tree)
;; '(1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
(encode '(SHA BOOM) huff-tree) ; '(1 1 1 0 1 1 0 1 1)


;;;--------------------------< ex 2.71 >--------------------------
;;; p220

;;============================
;; 1차 번역본의 오타인가? : 우씨!
;; 1,2,3,...,2^(n-1) 의 빈도
;;
;; 숫자가 곧 글자인 동시에 빈도수를 의미한다고 하면.
;; n:4
;; 글자 : 1 2 3 4 5 6 7 8
;; 순서 : 1 1   3 3   5 5
;;       ^^2 2 ^^6   ^^^7
;;      ^^^^^4     4
;;      ^^^^^^-----^6
;;      ^^^^^^^^^^^^^7
;;
;;              + (1,2,3,4,5,6,7,8):36
;;             / \
;;            /   \       
;;           /     \ 
;; (7,8):15 +       + (1,2,3,4,5,6):21
;;         / \     / \   
;;        7   8   /   \ 
;;               /     \
;;      (4,5):9 +       + (1,2,3,6):12 
;;             / \     / \             
;;            4   5   6   + (1,2,3):6   
;;                       / \            
;;                      3   + (1,2):3   
;;                         / \
;;                        1   2


;;============================
;; 1,2,4,...,2^(n-1) 의 빈도를 갖는 n개의 문자
;; n=5
;; 글자 : 1,2,4,8,16
;; 
;;      + (1,2,4,8,16):31
;;     / \
;;   16   + (1,2,4,8):15 
;;       / \             
;;      8   + (1,2,4):7   
;;         / \            
;;        4   + (1,2):3   
;;           / \
;;          2   1

;;============================
;; 1,2,4,...,2^(n-1) 의 빈도를 갖는 n개의 문자
;; 글자 : 1,2,4,8,16,32,64,128,256,512
;; n=10
;;
;;    + (1,2,4,8,16,32,64,128,256,512):1023
;;   / \
;;512   + (1,2,4,8,16,32,64,128,256):511
;;     / \
;;  256   +
;; ...
;;          + (1,2,4,8,16,32):63
;;         / \
;;       32   + (1,2,4,8,16):31
;;           / \ 
;;         16   + (1,2,4,8):15 
;;             / \             
;;            8   + (1,2,4):7   
;;               / \            
;;              4   + (1,2):3   
;;                 / \
;;                2   1

;; 가장 높은 빈도수의 문자 : 0
;; 가장 낮은 빈도수의 문자 : 1 ... 1 (n-1개)

;;
;; 1,2,...,2^(n-1) 까지의 핪은
;; 항상 2^n 보다 1이 작다
;;

;;;--------------------------< ex 2.72 >--------------------------
;;; p220

;; ex 2.71과 같은 형태의 허프만 나무에 대해서 인코딩한다면
;; 가장 높은 빈도수의 문자 : O(1)
;; 가장 낮은 빈도수의 문자 : O(n)
