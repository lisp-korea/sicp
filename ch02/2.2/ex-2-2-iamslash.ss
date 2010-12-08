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

