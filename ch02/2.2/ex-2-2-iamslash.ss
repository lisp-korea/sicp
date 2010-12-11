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
