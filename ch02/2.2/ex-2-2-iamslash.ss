;; -*- coding: utf-8 -*-

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; 리스트연산
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr iterms) (- n 1))))
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

;; ex.2.17
(define (last-pair l)
  (define (last-pair-iter l e)
    (if (null? l)
        e
        (last-pair-iter (cdr l) (car l))))    
  (last-pair-iter l null))

;; ex.2.18
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))
(define (reverse l)
  (define (reverse-iter l1 l2)
    (if (null? l1)
        l2
        (reverse-iter (cdr l1) (cons (car l1) l2))))
  (reverse-iter l null))

;; ex.2.19
(define (no-more? l)
  (if (null? l)
      #t
      #f))
(define (except-first-denomination l)
  (if (null? l)
      null
      (cdr l)))
(define (first-denomination l)
  (car l))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define us-coins2 (list 1 5 10 25 50))
(cc 100 us-coins)
(cc 100 us-coins2)
;;

;; ex.2.20
(define (same-parity a . l)
  (define (same-parity_ b_odd l1 l2)
    (if (null? l1)
        (reverse l2)
        (if (= b_odd (remainder (car l1) 2))
            (same-parity_ b_odd (cdr l1) (cons (car l1) l2))
            (same-parity_ b_odd (cdr l1) l2))))      
  (same-parity_ (remainder a 2) (cons a l) null))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;; ex.2.21
(define (square x)
  (* x x))
(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list (cdr items)))))
(define (square-list items)
  (map square items))
(square-list (list 1 2 3 4 5))

;; ex.2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))
(square-list (list 1 2 3 4 5))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))
(square-list (list 1 2 3 4 5))

;; iteration의 특성때문에 뒤집어 주어야 함
(define (reverse l)
  (define (reverse-iter l1 l2)
    (if (null? l1)
        l2
        (reverse-iter (cdr l1) (cons (car l1) l2))))
  (reverse-iter l null))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        (reverse answer)
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))
(square-list (list 1 2 3 4 5))

;; ex.2.23
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(define (for-each f l)
  (f (car l))
  (if (= (length l) 1)
      (f (car l))
      (for-each f (cdr l))))  
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;; ex.2.24

 ;; (1 (2 (3 4)))
 ;;      ^
 ;;    /   \
 ;;   1     ^ (2 (3 4))
 ;;       /   \
 ;;      2     ^ (3 4)
 ;;          /   \
 ;;         3     4

   ;; +---+---+  +---+---+
   ;; | * | *-+->| * | / |
   ;; +-+-+---+  +-+-+---+
   ;;   |          |   
   ;;   V          V      
   ;; +---+      +---+---+  +---+---+
   ;; | 1 |      | * | *-+->| * | / |
   ;; +---+      +-+-+---+  +---+---+
   ;;              |          |
   ;;              V          V
   ;;            +---+      +---+---+  +---+---+
   ;;            | 2 |      | * | *-+->| * | / |
   ;;            +---+      +-+-+---+  +-+-+---+
   ;;                         |          |
   ;;                         V          V
   ;;                       +---+      +---+
   ;;                       | 3 |      | 4 |
   ;;                       +---+      +---+

;; ex.2.25
(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (cdr l1)))))
(car (car l2))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))

;; ex.2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)

;; ex.2.27
(define (reverse l)
  (define (reverse-iter l1 l2)
    (if (null? l1)
        l2
        (reverse-iter (cdr l1) (cons (car l1) l2))))
  (if (pair? l)
       (reverse-iter l null)
       l))
(define (deep-reverse l)
  (if (pair? l)
       (reverse (map reverse l))
       l))
(define x (list (list 1 2) (list 3 4)))

;; ex.2.28
(define x (list (list 1 2) (list 3 4)))
(define (fringe l)
  (cond ((null? l) null)
        ((pair? l) (append (fringe (car l)) (fringe (cdr l))))
        (else (list l))))
(fringe x)
(fringe (list x x))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.29 ???

;; ex.2.29.a
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))
(left-branch (make-mobile 1 2))
(right-branch (make-mobile 1 2))
(branch-length (make-branch 1 2))
(branch-structure (make-branch 1 2))

;; ex.2.29.b
(define (total-weight m)
  (cond ((null? m) 0)
        ((not (pair? x)) m)
        (else (+ (total-weight (car m))
                 (total-weight (cdr x))))))
;; ex.2.29.c
;; ex.2.29.d


;; ex.2.30
(define (square-tree t)
  (cond ((null? t) null)
        ((not (pair? t)) (* t t))
        (else (cons (square-tree (car t))
                    (square-tree (cdr t))))))
(define (square-tree t)
  (map (lambda (st)
         (if (pair? st)
             (square-tree st)
             (* st st)))
       t))
(square-tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))

;; ex.2.31
(define (square x)
  (* x x))
(define (tree-map f t)
  (cond ((null? t) null)
        ((not (pair? t)) (f t))
        (else (cons (tree-map f (car t))
                    (tree-map f (cdr t))))))
(define (square-tree tree) (tree-map square tree))
(square-tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))
;; ex.2.32???
(define (subsets s)
  (if (null? s)
      null
      (let ((rest (subsets (cdr s))))
        (append rest (map <??> rest)))))
(subsets '(1 2 3))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;2.2.3 공통인터페이스로써 차례열의 쓰임새
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 차례열 연산
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons null (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
(define (even-fibs n)
  (accumulate cons
              null
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squared n)
  (accumulate cons
              null
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
(define (square x)
  (* x x))
(define (product-of-squared-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))
(product-of-squared-of-odd-elements (list 1 2 3 4 5))

(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.33
;; append이상하게 안되네...
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (square x)
  (* x x))
(define nil null)
(define (map p sequence)
  (accumulate (lambda
                  (x y)
                (cons (p x) y))
              nil
              sequence))
(define (append seq1 seq2)
  accumulate cons seq2 seq1)
(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y)) 0 sequence))

(map square (list 1 2 3 4 5))
(append (list 1 2 3) (list 4 5 6))
(length (list 1 2 3 4 5))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.34
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.35
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (not (pair? x))
                             1
                             (count-leaves x))) t)))
(count-leaves `(1 2 (2 3 (1 2))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.36
(define l `((1 2 3) (4 5 6) ( 7 8 9) (10 11 12)))
(map car l)
(map cdr l)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(accumulate-n + 0 l)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.37
(define m1 `((1 2 3 4) (4 5 6 6) (6 7 8 9 )))
(define m2 `((1 2 3) (4 5 6) (6 7 8) (7 8 9)))
(define v  `(1 2 3 4))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v)) 
        m))
(matrix-*-vector m1 v)

(define (transpose mat)
  (accumulate-n cons null mat))
(transpose m1)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector cols m-row)) 
          m)))
(matrix-*-matrix m1 m2)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
  
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list null (list 1 2 3))
(fold-left list null (list 1 2 3))
;; 교환법칙이 성립하는 op를 사용하면 결과는 같다.
(fold-right + 1 (list 1 2 3))
(fold-left + 1 (list 1 2 3))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex.2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))
(reverse `(1 2 3 4))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))
(reverse `(1 2 3 4))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 겹친 매핑
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate append
            null
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 5)))
(map (lambda (i)
       (map (lambda (j) (list i j))
            (enumerate-interval 1 (- i 1))))
     (enumerate-interval 1 5)) ;; nested mapping을 이해하기 위한 첫걸음, C의 이중loop
;; (for i=0; i<5; ++i)
;;  (for j=0; j<5; ++j)
;;   (i,j)
;;
(map (lambda (i)
       (map (lambda (j) (list i j))
            (enumerate-interval 0 2)))
       (enumerate-interval 0 2))
(accumulate append
            null
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 0 2)))
                 (enumerate-interval 0 2)))            
(define (loop-2 i1 i2)
  (map (lambda (i)
         (map (lambda (j) (list i j))
              (enumerate-interval 0 i2)))
       (enumerate-interval 0 i1)))
(loop-2 5 5)
(define (loop-3 i1 i2 i3)
  (map (lambda (i)
         (map (lambda (j)
                (map (lambda (k) (list i j k))
                     (enumerate-interval 0 i3))
                (enumerate-interval 0 i2)))X               
         (enumerate-interval 0 i1))))
(loop-3 5 5 5)  

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))
(flatmap
 (lambda (i)
   (map (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1))))
 (enumerate-interval 1 5))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(prime-sum-pairs 10)

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
