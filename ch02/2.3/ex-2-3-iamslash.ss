;; -*- coding: utf-8 -*-

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.1 따옴표연산
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.54
(define (equal? l1 l2)
  (define (eq? a b)
    (= a b))
  (cond ((and (null? l1) (null? l2))
         #t)
        ((and (pair? l1) (pair? l2))
         (if (and (eq? (car l1) (car l2))
                  (equal? (cdr l1) (cdr l2)))
             #t
             #f))
        ((and (not (pair? l1)) (not (pair? l2)))
         (eq? l1 l2))
        (else #f)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.55
(car ''abracadabra) ; 'abracadabra 는 pair이다. 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.2. 연습: 글자 식의 미분
(variable? e)
(same-variable? v1 v2)

(sum? e)
(addend e)
(augend e)
(make-sum a1 a2)

(product? e)
(multiplier e)
(multiplicand e)
(make-product m1 m2)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplicand p) (cadr p))
(define (multiplier p) (caddr p))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(define (=number? exp num)
  (and (number? exp) (= exp num)))
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




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.56
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplicand p) (cadr p))
(define (multiplier p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))
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

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))
(define (make-exponentiation b e)
  (list '** b e))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp) (- (exponent exp) 1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(make-exponentiation 3 3)
(deriv '(exp x 3) 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.57
(define (addend x)
  (if (null? (cdddr x)) (caddr x)
      (cons '+ (cddr x))))
(define (multiplicand x)
  (if (null? (cdddr x)) (caddr x)
      (cons '* (cddr x))))
(addend '(+ 1 2 3))
(multiplicand '(* 1 2 3 4 5 6))
      
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.58 ;; 잘안됩니다.


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.58.a
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplicand p) (car p))
(define (multiplier p) (caddr p))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
(deriv '(x + (3 * (x + (y + 2)))) 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.58.b
(define (augend x)
  (if (null? (cdddr x)) (caddr x)
      (cond ((eq? (cadddr x) '+) (make-sum (caddr x) (cadddr (cdr x))))
            ((eq? (cadddr x) '*) (make-product (caddr x) (cadddr (cdr x)))))))
(define (multiplicand x)
  (if (null? (cdddr x)) (caddr x)
      (cond ((eq? (cadddr x) '+) (make-sum (caddr x) (cadddr (cdr x))))
            ((eq? (cadddr x) '*) (make-product (caddr x) (cadddr (cdr x)))))))

(augend '(x + y + z))
(multiplicand '(x + 3 * z))
(deriv '(x + 3 * (x + y + 2)) 'x) ;; ???


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.3. 연습: 집합을 나타내는 방법

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 차례 없는 리스트로 표현한 집합

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(element-of-set? '1 '(1 2 3 4 5))
(adjoin-set '1 '(2 3 4 5))
(intersection-set '(1 2 3) '(1 4 5))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

(union-set '(1 2 3) '(1 4 5))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.60

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.61

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.62

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.63

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.64

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.65

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.66

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.4. 연습: 허프만 인코딩 나무
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left right
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 디코딩 프로시저
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 무게가 있는 원소들의 집합
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.68 
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol x tree)
  (define (encode-symbol-core branch)
    (cond ((leaf? branch) '())
          ((element-of-set? x (symbols (left-branch branch)))
           (cons 0 (encode-symbol-core (left-branch branch))))
          (else
           (cons 1 (encode-symbol-core (right-branch branch))))))
  (if (element-of-set? x (symbols tree))
      (encode-symbol-core tree)
      (error "tree is not related to symbols" x)))
(encode '(A D A B B C A) sample-tree)
(symbols sample-tree)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.69 
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge set)
  (define (core set)
    (cond ((null? (cdr set)) (car set))
          (else (core (adjoin-set
                       (make-code-tree (car set) (cadr set))
                       (cddr set))))))
  (core set))
(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
(successive-merge '((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.70 
(define lyrics-tree
  (generate-huffman-tree '((BOOM 1) (WAH 1) (A 2) (GET 2) (JOB 2)
                           (SHA 3) (YIP 9) (NA 16))))
(define lyrics-msg
  '(GET A JOB

SHA NA NA NA NA NA NA NA NA

GET A JOB

SHA NA NA NA NA NA NA NA NA

WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP

SHA BOOM))

(encode lyrics-msg lyrics-tree)
(symbols lyrics-tree)

;; 8낱말로 이루어진 글 집합을 길이가 정해진 코드로 인코딩한다고 하면
;; lyrics-msg를 인코딩하는데 드는 가장 적은 비트는 "108"비트 이다.
;; (+ (* (* 3 1) 2) (* 3 (* 3 2)) (* 3 3) (* 3 9) (* 3 16) )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.71

;; n=5
;; 가장많이 나오는 글자 인코딩 비트수 1
;; 가장많이 나오는 글자 인코딩 비트수 4
(generate-huffman-tree
 '((A 1) (B 2) (C 4) (D 8 ) (E 16)))

(define t1 (generate-huffman-tree
 '((A 1) (B 2) (C 4) (D 8 ) (E 16))))
(define m1 '(E))
(define m2 '(A))

(encode m1 t1)
(encode m2 t1)
        
;; n=10
;; 가장많이 나오는 글자 인코딩 비트수 1
;; 가장많이 나오는 글자 인코딩 비트수 9
(generate-huffman-tree
 '((A 1) (B 2) (C 4) (D 8 ) (E 16)
   (F 32) (G 64) (H 128) (I 256) (J 512)))
(define t2 (generate-huffman-tree
 '((A 1) (B 2) (C 4) (D 8 ) (E 16)
   (F 32) (G 64) (H 128) (I 256) (J 512))))
(define m3 '(E))
(define m4 '(A))

(encode m3 t2)
(encode m4 t2)

;; 결론
;; 가장많이 나오는 글자 인코딩 비트수 1
;; 가장많이 나오는 글자 인코딩 비트수 n-1


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.72
Θ(1) ;; 가장많이 나오는 글자 인코딩 비트수 1
Θ(n) ;; 가자많이 나오는 글자 인코딩 비트수 n-1