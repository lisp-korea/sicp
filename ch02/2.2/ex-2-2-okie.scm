(cons 1 2)
(cons (cons 1 2) (cons 3 4))
(cons (cons 1 (cons 2 3)) 4)

(cons 1 (cons 2 (cons 3 (cons 4 nil))))

(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (=n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items))))
  
(define odds (list 1 3 5 7))

(length odds)

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(append squares odds)

(append odds squares)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; ex 2.17
(list-pair (list 23 72 149 34))

(define (list-pair list)
  (if (null? (cdr (cdr list)))
      (cdr list)
      (list-pair (cdr list))))
      
      
; ex 2.18

(reverse (list 1 4 9 16 25))

(define (reverse list)
  (if (null? (cdr list))
      list
      (cons (reverse (cdr list)) (car list))))

(define (reverse list)
  (define (reverse-iter list result)
    (if (null? list)
        result
        (reverse-iter (cdr list) (cons (car list) result))))
  (reverse-iter list '()))

; ex 2.19
(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (except-first-denomination list)
  (cdr list))

(define (first-denomination list)
  (car list))

(define (no-more? list)
  (null? list))        

(define us-coins2 (list 1 5 10 25 50))

(cc 100 us-coins2)

;; no effect

; ex 2.20  
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(define (same-parity . n)
  (define (same-parity-inter l parity result)
    (if (null? l)
        result
        (if (boolean=? parity (even? (car l)))
            (same-parity-inter (cdr l) parity (append result (list (car l))))
            (same-parity-inter (cdr l) parity result))))
  (same-parity-inter n (even? (car n)) '()))

; list mapping

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))

(map (lambda (x y) (+ x (* 2 y)))
     (list 1 2 3)
     (list 4 5 6))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

; ex 2.21
(square-list (list 1 2 3 4))

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x))
       items))

; ex 2.22
(define (square x)
  (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))
               
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

; (cons (list 1 2) 3) ==> ((1 2) . 3)
; (cons 1 (list 2 3)) ==> (1 2 3)

; ex 2.23
(for-each (lambda (x)
                 (newline)
                 (display x))
          (list 57 321 88))

(define (for-each proc l)
  (proc (car l))
  (if (not (null? (cdr l)))
      (for-each proc (cdr l))
      #f))
