;; Ex 2.33 
  
 ;; Accumulates the result of the first and the already-accumulated 
 ;; rest. 
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
  
 (define nil '())  ;; stupid environment ... 
  
 ;; a. map 
  
 (define (my-map proc sequence) 
   (accumulate (lambda (first already-accumulated) 
                 (cons (proc first) already-accumulated)) 
               nil 
               sequence)) 
  
 ;; Test: 
  
 (my-map square (list)) 
 (my-map square (list 1 2 3 4)) 
  
  
 ;; b. append 
  
 (define (my-append list1 list2) 
   (accumulate cons 
               list2 
               list1)) 
  
 ;; Test: 
  
 (append (list 1 2 3) (list 4 5 6))  ;; checking order. 
 (my-append (list 1 2 3) (list 4 5 6)) 
  
  
 ;; c. length 
  
 (define (my-length sequence) 
   (accumulate (lambda (first already-acc) 
                 (+ 1 already-acc)) 
               0 
               sequence)) 
  
 ;; Test: 
 (length (list 1 2 3 (list 4 5))) 
 (my-length (list 1 2 3 (list 4 5)))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;ex2.34
  ;; Accumulates the result of the first and the already-accumulated 
 ;; rest. 
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
  
 (define nil '())  ;; stupid environment ... 
  
 (define (horner-eval x coefficient-sequence) 
   (accumulate (lambda (this-coeff accum-sum) 
                 (+ this-coeff 
                    (* x accum-sum))) 
               0 
               coefficient-sequence)) 
  
 (horner-eval 2 (list 1 3 0 5 0 1)) 

 ;;;;;;;;;;;;;;ex 2.34
 ;; Accumulates the result of the first and the already-accumulated 
 ;; rest. 
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
 (accumulate + 0 (list 1 2 3 4 5)) 
 (accumulate cons nil (list 2 3 4 5 6)) 
  
 ;; Pulls out all the leaves of the tree, returns as a flat list. 
 (define (enumerate-tree tree) 
   (cond ((null? tree) nil) 
         ((not (pair? tree)) (list tree)) 
         (else (append (enumerate-tree (car tree)) 
                       (enumerate-tree (cdr tree)))))) 
  
  
 (define (count-leaves t) 
   (accumulate + 
               0 
               (map (lambda (x) 1)  
                    (enumerate-tree t)))) 
  
 ;; Usage 
 (define tree (list 1 2 3 (list 4 5 (list 6 7)))) 
 (count-leaves tree)  ;; => 7 

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ex2.36
 (define (select-cars sequence) 
   (map car sequence)) 
  
 (define (select-cdrs sequence) 
   (map cdr sequence)) 
  
 ;; Test 
 (define t (list (list 1 2 3) (list 40 50 60) (list 700 800 900))) 
 (select-cars t) 
 (select-cdrs t) 
  
  
 ;; Accumulates the result of the first and the already-accumulated 
 ;; rest. 
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
  
 ;; accumulate-n 
 (define (accumulate-n op init sequence) 
   (define nil '()) 
   (if (null? (car sequence)) 
       nil 
       (cons (accumulate op init (map car sequence)) 
             (accumulate-n op init (map cdr sequence))))) 
  
 ;; Usage: 
 (accumulate-n + 0 t) 

 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ex 2.37
 ;; Accumulates the result of the first and the already-accumulated 
 ;; rest. 
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
  
 ;; accumulate-n 
 (define (accumulate-n op init sequence) 
   (define nil '()) 
   (if (null? (car sequence)) 
       nil 
       (cons (accumulate op init (map car sequence)) 
             (accumulate-n op init (map cdr sequence))))) 
  
  
 (define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 
  
  
 (define (dot-product v1 v2) 
   (accumulate + 0 (map * v1 v2))) 
  
 ;; Test 
 (dot-product (list 1 2 3) (list 4 5 6)) 
  
  
 ;; a. 
 (define (matrix-*-vector m v) 
   (map (lambda (m-row) (dot-product m-row v)) 
        m)) 
  
 ;; Test 
 (matrix-*-vector matrix (list 2 3 4 5)) 
  
  
 ;; b. 
 (define nil '()) 
 (define (transpose m) 
   (accumulate-n cons nil m)) 
  
 ;; Test 
 (transpose matrix) 
  
  
 ;; c. 
 (define (matrix-*-matrix m n) 
   (let ((n-cols (transpose n))) 
     (map (lambda (m-row) 
            (map (lambda (n-col)  
                   (dot-product m-row n-col))  
                 n-cols)) 
          m))) 
  
 ;; But the inner map is just matrix-*-vector, so here's better: 
 (define (matrix-*-matrix m n) 
   (let ((n-cols (transpose n))) 
     (map (lambda (m-row) (matrix-*-vector n-cols m-row)) 
          m))) 
  
 ;; Test 
 (matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2))) 

 ;;;;;;;;;;;;;;;;;ex2.38
  ;; Accumulates the result of the first and the already-accumulated 
 ;; rest. 
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
 (define (fold-right op initial sequence) 
   (accumulate op initial sequence)) 
  
  
 (define (fold-left op initial sequence) 
   (define (iter result rest) 
     (if (null? rest) 
         result 
         (iter (op result (car rest)) 
               (cdr rest)))) 
   (iter initial sequence)) 
 
;;;;;;;;;;;;;; ex2.39
 
  (define (fold-right op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
  
 (define (fold-left op initial sequence) 
   (define (iter result rest) 
     (if (null? rest) 
         result 
         (iter (op result (car rest)) 
               (cdr rest)))) 
   (iter initial sequence)) 

 
 (define (reverse-using-right items) 
   (fold-right (lambda (first already-reversed) 
                 (append already-reversed (list first))) 
               nil 
               items)) 
  
 (define (reverse-using-left items) 
   (fold-left (lambda (result first) (cons first result)) 
              nil 
              items)) 
  
 ;; Test 
 (define items (list 1 2 3 4 5)) 
 (reverse-using-right items) 
 (reverse-using-left items) 


 ;;;;;;;;;;;;;;;;;;;;;;;;;;ex 2.40
 ;; Supporting functions: 
  
 (define nil '()) 
  
 (define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 
  
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
 (define (enumerate-interval low high) 
   (if (> low high) 
       nil 
       (cons low (enumerate-interval (+ low 1) high)))) 
  
 (define (flatmap proc seq) 
   (accumulate append nil (map proc seq))) 
  
 (define (prime? x) 
   (define (test divisor) 
     (cond ((> (* divisor divisor) x) true) 
           ((= 0 (remainder x divisor)) false) 
           (else (test (+ divisor 1))))) 
   (test 2)) 
  
 (define (prime-sum? pair) 
   (prime? (+ (car pair) (cadr pair)))) 
  
 (define (make-sum-pair pair) 
   (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))) 
  
 ;; ------------ 
  
 ;; The answer ... it's just the top of page 123, pulled into a new 
 ;; function (with flatmap): 
 (define (unique-pairs n) 
   (flatmap (lambda (i)  
              (map (lambda (j) (list i j)) 
                   (enumerate-interval 1 (- i 1)))) 
            (enumerate-interval 1 n))) 
  
 ;; Test: 
 (unique-pairs 5) 
  
  
 (define (prime-sum-pairs n) 
   (map make-sum-pair 
        (filter prime-sum? (unique-pairs n)))) 
  
 ;; Test: 
 (prime-sum-pairs 6) 
 ;; => ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11)) 

 
 ;;;;;;;;;;;;;;;;ex 2.41
 
 ;; Supporting functions: 
  
 (define nil '()) 
  
 (define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 
  
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
 (define (enumerate-interval low high) 
   (if (> low high) 
       nil 
       (cons low (enumerate-interval (+ low 1) high)))) 
  
  
 (define (flatmap proc seq) 
   (accumulate append nil (map proc seq))) 
  
 ;; Here's the unique-pairs from the chapter: 
 (define (unique-pairs n) 
   (flatmap (lambda (i) 
              (map (lambda (j) (list i j)) 
                   (enumerate-interval 1 (- i 1)))) 
            (enumerate-interval 1 n))) 
  
  
 ;; We need to make triples (i j k).  The following will do: 
  
 (define (unique-triples n) 
   (flatmap (lambda (i) 
              (flatmap (lambda (j) 
                         (map (lambda (k) (list i j k)) 
                              (enumerate-interval 1 (- j 1)))) 
                       (enumerate-interval 1 (- i 1)))) 
            (enumerate-interval 1 n))) 
;;;;;;;;;;;;;;;;;;;;;;;;ex2.42
  ;; Queens puzzle. 
  
 ;; --------------------------- 
 ;; Support. 
  
 (define nil '()) 
  
 ;; Builds a list of sequence items that pass the predicate. 
 (define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 
  
 ;; Accumulates the result of the first and the already-accumulated 
 ;; rest. 
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
 (Define (enumerate-interval low high) 
   (if (> low high) 
       nil 
       (cons low (enumerate-interval (+ low 1) high)))) 
  
 (define (flatmap proc seq) 
   (accumulate append nil (map proc seq))) 
  
 (define (select-item n items) 
   (cond ((null? items) (error "out of items?" n)) 
         ((= 1 n) (car items)) 
         (else (select-item (- n 1) (cdr items))))) 
  
  
 ;; --------------------------- 
  
 ;; The data abstraction.  A queen's position can be given as (cons row 
 ;; column).  A position is just a list of queens. 
  
 ;; Accessors: 
 (define (make-empty-board) '()) 
  
 ;; Queen position: 
 (define (make-queen row column) (cons row column)) 
 (define (queen-row q) (car q)) 
 (define (queen-column q) (cdr q)) 
  
 ;; Really should check for queen in same row, column. 
 (define (add-queen row column queens-already-placed) 
   (cons (make-queen row column) queens-already-placed)) 
  
  
 ;; Returns nil if no queen in that column. 
 (define (get-queen-in-column column queens) 
   (define (first-queen qs) (car qs)) 
   (cond ((null? queens) nil) 
         ((= (queen-column (first-queen queens)) column) 
          (first-queen queens)) 
         (else (get-queen-in-column column (cdr queens))))) 
  
  
 ;; --------------------------- 
 ;; Queen safety. 
  
 ;; A queen will be safe if it is in a) its own column, b) its own row, 
 ;; and c) its own diagonals.  a) and b) are easy; as for c), we just 
 ;; need to check that the slope of the line formed by the two queens 
 ;; is 1 or -1, or that the absolute value of the row difference is the 
 ;; same as that of the column difference. 
 (define (on-same-column q1 q2) (= (queen-column q1) (queen-column q2))) 
 (define (on-same-row q1 q2) (= (queen-row q1) (queen-row q2))) 
 (define (on-same-diag q1 q2) 
   (= (abs (- (queen-row q1) (queen-row q2))) 
      (abs (- (queen-column q1) (queen-column q2))))) 
  
 (define (safe-from-attack q1 q2) 
   (or (and (on-same-row q1 q2) 
            (on-same-column q1 q2))  ;; q can't attack itself! 
  
       (and (not (on-same-column q1 q2)) 
            (not (on-same-row q1 q2)) 
            (not (on-same-diag q1 q2))))) 
  
  
 ;; Returns true if the queen in the given column is safe from attack 
 ;; by all other queens (ie no other queens are on the same row or 
 ;; diagonal). 
 (define (safe? column all-queens) 
   (let ((q-in-col (get-queen-in-column column all-queens))) 
     ;; For some reason, couldn't just pass "and" as the operator, 
     ;; would get error: ";Syntactic keyword may not be used as an 
     ;; expression: #f". 
     (accumulate (lambda (a b) (and a b)) 
                 true 
                 (map (lambda (q) (safe-from-attack q-in-col q)) 
                      all-queens)))) 
  
  
 ;; ------------------------- 
 ;; <TESTING> 
  
 (define (ensure expected-true message) 
   (if (not expected-true) 
       (error "Failure: " message))) 
  
  
 ;; Defining the current position for columns 1 to 3: 
 ;; --3. 
 ;; 1--. 
 ;; -2-. 
  
 (define base-pos (add-queen 3 3  
                             (add-queen 1 2  
                                        (add-queen 2 1  
                                                   (make-empty-board))))) 
  
 (get-queen-in-column 2 base-pos) 
 (get-queen-in-column 1 base-pos) 
 (get-queen-in-column 4 base-pos) 
  
 ;; Tests 
 (define q1-1 (make-queen 1 1)) 
 (define q1-3 (make-queen 1 3)) 
 (define q3-3 (make-queen 3 3)) 
 (define q2-2 (make-queen 2 2)) 
 (define q4-6 (make-queen 4 6)) 
 (define q5-5 (make-queen 5 5)) 
  
 (ensure (on-same-column q1-3 q3-3) "both in 3rd col") 
 (ensure (not (on-same-column q2-2 q3-3)) "diff cols") 
 (ensure (on-same-row q1-3 q1-1) "1st row") 
 (ensure (not (on-same-row q1-1 q3-3)) "diff row") 
 (ensure (on-same-diag q1-1 q3-3) "asc diag") 
 (ensure (on-same-diag q1-3 q4-6) "asc diag 1-3") 
 (ensure (on-same-diag q5-5 q4-6) "desc diag 5-5") 
 (ensure (not (on-same-diag q4-6 q1-1)) "diff diag") 
 (ensure (safe-from-attack q1-1 q4-6) "safe") 
 (ensure (not (safe-from-attack q1-1 q5-5)) "same diag") 
 (ensure (not (safe-from-attack q4-6 q5-5)) "desc diag") 
 (ensure (safe-from-attack q1-1 q1-1) "queen can't attack itself") 
  
 (ensure (safe? 3 base-pos) "on own row, col, and diag") 
 (ensure (not (safe? 2 base-pos)) "on desc diag with 1") 
 (ensure (not (safe? 1 base-pos)) "on desc diag with 2") 
  
 ;; </TESTING> 
 ;; ------------------------- 
  
  
 ;; The function. 
  
 (define (queens board-size) 
   (define row-numbers (enumerate-interval 1 board-size)) 
   (define empty-board (make-empty-board)) 
  
   (define (safely-place-queen-in-column column) 
     (if (= column 0) 
         (list empty-board) 
         (filter 
          (lambda (all-queens) (safe? column all-queens)) 
          (flatmap 
           (lambda (queens-already-placed) 
             (map (lambda (row) 
                    (add-queen row column queens-already-placed)) 
                  row-numbers)) 
           (safely-place-queen-in-column (- column 1)))))) 
  
   (safely-place-queen-in-column board-size)) 
  
  
 ;; Usage: 
 (queens 8) 

 
 ;;;;;;;;;;;;ex 2.43
 
 답만 배끼는 것도 쉽지 않군요. OZL....