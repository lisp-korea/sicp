;ex 2.17
(define (last-pair items) 
   (define (iter items result) 
     (if (null? items) 
         result 
         (iter (cdr items) items))) 
   (iter items items)) 

(print "ex2.17")
(newline)
(last-pair (list 23 72 149 34))

;ex 2.18
 (define nil '()) 
 (define (reverse items) 
   (if (null? (cdr items)) 
       items 
       (append (reverse (cdr items)) 
               (list (car items)))))
  
 (print "ex2.18")
(newline)
 (reverse (list 1 4 9 16 25)) 

 
 ;ex2.19
 (define (first-denomination denominations) (car denominations)) 
 (define (except-first-denom denominations) (cdr denominations)) 
 (define (no-more? denominations) (null? denominations)) 
  
 (define (cc amount denominations) 
   (cond  
    ;; If there's no change left, we have a solution 
    ((= amount 0) 1) 
     
    ;; If we're gone -ve amount, or there are no more kinds of coins 
    ;; to play with, we don't have a solution. 
    ((or (< amount 0) (no-more? denominations)) 0) 
     
    (else 
     ;; number of ways to make change without the current coin type 
     ;; plus the number of ways after subtracting the amount of the 
     ;; current coin. 
     (+ (cc amount (except-first-denom denominations)) 
        (cc (- amount  
               (first-denomination denominations))  
            denominations))))) 
  
(print "ex 2.19")
(newline)
 (cc 100 (list 50 25 10 5 1)) 
 ;; 292 
  
; (cc 100 (list 100 50 20 10 5 2 1 0.5)) 
 ;; 104561 ... so many time
 
 ;ex 2.20
  (define (same-parity first . rest) 
   (define (congruent-to-first-mod-2? a) 
     (= (remainder a 2) (remainder first 2))) 
  
   (define (select-same-parity items) 
     (if (null? items)  
         items 
         (let ((curr (car items)) 
               (select-rest (select-same-parity (cdr items)))) 
           (if (congruent-to-first-mod-2? curr) 
               (cons curr select-rest) 
               select-rest)))) 
  
   (cons first (select-same-parity rest))) 
  
 ;; an alternative implementation by andras: 
 (define (same-parity a . l) 
         (define (sp-builder result tail) 
         (if (null? tail) 
         result 
         (if (even? (+ a (car tail))) 
         ;;test for same parity 
         ;;if the current beginning of the rest (car tail) is the same parity as "a", then it is appended to the result, else the result is left untouched 
         (sp-builder (append result (list (car tail))) (cdr tail)) 
         (sp-builder result (cdr tail))))) 
         (sp-builder (list a) l)) 

 (print "ex2.20")
 (newline)
 ;; Usage: 
 (same-parity 1 2 3 4 5 6 7) 
 ;; (1 3 5 7) 
  
 (same-parity 2 3 4 5 6 7 8) 
 ;; (2 4 6 8) 

 ;ex2.21
(define (square-list items) 
   (if (null? items) 
       items 
       (cons (square (car items)) (square-list (cdr items))))) 

(define (square x)
   (* x x)
)

(print "ex2.21")
      (newline)
 (square-list (list 1 2 3 4)) 
  
 (define (sq2 items) 
   (map (lambda (x) (square x)) items)) 
  
 (sq2 (list 1 2 3 4))
 
 ;ex 2.22
  (define (square-list items) 
   (define (iter things answer) 
     (if (null? things) 
         answer 
         (iter (cdr things) 
               (cons (square (car things)) answer)))) 
   (iter items nil)) 
  
 (square-list (list 1 2 3 4)) 
  
 ;; The above doesn't work because it conses the last item from the 
 ;; front of the list to the answer, then gets the next item from the 
 ;; front, etc. 
  
 (define (square-list items) 
   (define (iter things answer) 
     (if (null? things) 
         answer 
         (iter (cdr things) 
               (cons answer (square (car things)))))) 
   (iter items nil)) 
  
 (print "ex 2.22")
 (newline)
 (square-list (list 1 2 3 4)) 
  
 ;; This new-and-not-improved version conses the answer to the squared 
 ;; value, but the answer is a list, so you'll end up with (list (list 
 ;; ...) lastest-square).
 
 ;ex 2.23
 (define (for-each proc items) 
   (let ((items-cdr (cdr items))) 
     (proc (car items)) 
     (if (not (null? items-cdr)) 
         (for-each proc items-cdr) 
         true))) 

(print "ex2.23")
(newline)
 (for-each (lambda (x) (newline) (display x)) (list 1 2 3 4)) 

 ;ex2.24
 ;Tree:

; (1 (2 (3 4)))
;      ^
;    /   \
;   1     ^ (2 (3 4))
;       /   \
;      2     ^ (3 4)
;          /   \
;         3     4

 ;ex 2.25
 (newline)
   (define items (list 1 3 (list 5 7) 9))
   (print "ex2.25-1")
   (newline)
  (car (cdr (car (cdr (cdr items)))))

   (print "ex2.25-2")
   (newline)
   (car (car (list (list 7))))
   (print "ex2.25-3")
   (newline)
(define a (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr a))))))))))))

;ex2.26

  (define x (list 1 2 3))
  (define y (list 4 5 6))
  (print "ex2.26")
  (newline)
(append x y)
(cons x y)
(list x y)

;ex2.27
 (define (deep-reverse tree) 
   (define (iter t result) 
     (cond ((null? t) result) 
           ((not (pair? (car t))) 
            (iter (cdr t) (cons (car t) result))) 
           (else  
            (iter (cdr t) (cons (deep-reverse (car t)) result))))) 
   (iter tree '())) 
  
 (print "ex2.27")
 (newline)
(deep-reverse '((1 2) (3 4))) 
; '((4 3) (2 1)) 
(deep-reverse '(1 2 (3 4) 5 (6 (7 8) 9) 10)) 
; '(10 (9 (8 7) 6) 5 (4 3) 2 1) 

;ex 2.28

