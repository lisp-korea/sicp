
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))

;; 2.2.3 << 공통 인터페이스로써 차례열의 쓰임새 >>

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)		
	((not (pair? tree)) (if (odd? tree) (square tree) 0))
	(else (+ (sum-odd-squares (car tree))                
		 (sum-odd-squares (cdr tree))))))

(sum-odd-squares '((1 2) (3) 4 5 6))


(define nil '())

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence)) (cons (car sequence)
					  (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))



(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))

(* 1 (* 2 (* 3 (* 4 (* 5 1)))))


(accumulate cons nil (list 1 2 3 4 5))


(define (enumerate-interval low high)
  (if (> low high) nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(enumerate-tree '(1 2 (3 (4) 5) (6)))
(enumerate-interval 5 10)





(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))
;;  accumulate        map       filter         enumerate

(sum-odd-squares '(1 2 (3 (4) 5) ((6))))

(map square (filter odd? '(1 2 3 4 5 6)))


(define (even-fibs n)
  (accumulate cons nil (filter even? (map fib (enumerate-interval 0 n)))))

(even-fibs 10)

(define (list-fib-squares n)
  (accumulate cons nil (map square (map fib
					(enumerate-interval 0 n)))))


(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))




(define (salary-of-highest-paid-programmer records)
  (accumulate max 0 (map salary (filter programmer? records))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 연습문제 2.33


;; (define (accumulate op initial sequence)
;;   (if (null? sequence) initial
;;       (op (car sequence)
;; 	  (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(my-map square '(1 2 3))



(my-map square (list 1 2 3 4))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

 '(2 4)

(if (null? <????>) <??>
    (cons (car <????>)
	  (accumulate cons <??> (cdr <????>))))

(append '(1 2) '(3 4)) ;=> '(1 2 3 4)

(my-append '(1 2) '(3 4))

(cons 1 (accumulate cons '(3 4) '(2)))
(cons 1 (cons 2 (accumulate cons '(3 4) '())))
(cons 1 (cons 2 '(3 4)))
(cons 1 '(2 3 4))
'(1 2 3 4)

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))



(define (my-length sequence)
  (accumulate <??> 0 sequence))


(my-length '(1 2 3))

(if (null? '(1 2 3)) 0
    (<??> 1 (accumulate cons <??> '(2 3))))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;; 연습문제 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) <??>)
	      0
	      coefficient-sequence))



;; x = 2 , 1 + 3x + 5x^3 + x^5
(herner-eval 2 (list 1 3 0 5 0 1))

(lambda (a b) (+ a (* x b)))


(if (null? sequence) '(1 3 0 5 0 1)
    ((lambda (a b) <??>) 1 (accumulate (lambda (a b) <??>) 0 '(3 0 5 0 1))))


((lambda (a b) <??>) 1
 (if (null? sequence) '(3 0 5 0 1)
     ((lambda (a b) <??>) 3 (accumulate (lambda (a b) <??>) 0 '(0 5 0 1)))))
                     .
                     .
                     .

((lambda (a b) <??>) 1
              ((lambda (a b) <??>) 3
	            ((lambda (a b) <??>) 0
		         ((lambda (a b) <??>) 5
			       ((lambda (a b) <??>) 0
				    ((lambda (a b) <??>) 1 0))))))



;; 연습문제 2.35

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(count-leaves '((1 (3) 2) 3 ((4) 5 6 7)))

(define (count-leaves x)
  (accumulate + 0 (map (lambda (a)
			 (if (not (pair? a)) 1
			     (length a))) x)))

(define (count-leaves x)
  (accumulate +  0 (map (lambda (a)
			  (if (not (pair? a)) 1
			      (count-leaves a))) x)))

;; 연습문제 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init <??>)
	    (accumulate-n op init <??>))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
;=> (22 26 30)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))



;; 연습문제 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define m '((1 2) (3 4)))
(define v '(1 2))

(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate + 0 (map * x v))) m))

(matrix-*-vector m v)


(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (accumulate cons nil (matrix-*-vector cols x))) m)))

(matrix-*-matrix m m)


;; 연습문제 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-left / 1 (list 1 2 3))
(fold-right / 1 (list 1 2 3))

(fold-left list nil (list 1 2 3))
(fold-right list nil (list 1 2 3))


;(fold-left / 1 (list 1 2 3))
;(/ (/ (/ 1 1) 2) 3)
;(fold-right / 1 (list 1 2 3))
;(/ 1 (/ 2 (/ 3 1)))

;; (fold-left list nil (list 1 2 3))
;; (list (list (list nil 1) 2) 3)
;; (fold-right list nil (list 1 2 3))
;; (list 1 (list 2 (list 3 nil)))

;; fold-left 와 fold light 는 연산 순서가 달라진다.

(= (fold-left - 0 (list 1 2 3 4)) (fold-right - 0 (list 1 2 3 4)))
;but!
(= (fold-left + 0 (list 1 2 3 4)) (fold-right + 0 (list 1 2 3 4)))
(= (fold-left * 1 (list 1 2 3 4)) (fold-right * 1 (list 1 2 3 4)))


;; 연습문제 2.39

(define (my-reverse sequence)
  (fold-right (lambda (x y) (append y (cons x nil))) nil sequence))

;(my-reverse '(1 2 3 4))
; (append                                                                   (cons 1 nil))
;        (append                                               (cons 2 nil))
;               (append                          (cons 3 nil))
;                      (append nil (cons 4 nil))

;(append (append (append (append nil (cons 4 nil)) (cons 3 nil)) (cons 2 nil)) (cons 1 nil))

(define (my-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; (iter ((lambda (x y) (cons y x)) nil 1) '(2 3 4))
;; (iter ((lambda (x y) (cons y x)) '(1) 2) '(3 4))
;; (iter ((lambda (x y) (cons y x)) '(2 1) 3) '(4))
;; (iter ((lambda (x y) (cons y x)) '(3 2 1) 4) '())
;; '(4 3 2 1)





;; 겹친 맵핑


(accumulate append nil
	    (map (lambda (i)
		   (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
		 (enumerate-interval 1 n)))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum?
			     (flatmap
			      (lambda (i)
				(map (lambda (j) (list i j))
				     (enumerate-interval 1 (- i 1))))
			      (enumerate-interval 1 n)))))


;; (map (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
;;      (enumerate-interval 1 6))

;; (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
;;      (enumerate-interval 1 6))



(define (permutations s)
  (if (null? s) (list nil)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(permutations '(1 2 3))

(flatmap (lambda (x)
	   (map (lambda (p) (cons x p))
		(permutations (remove x s))))
	 '(1 2 3))



;; 연습문제 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))


;; 연습문제 2.41

(define (unique-triple? triple)
  (cond ((= (car triple) (cadr triple)) #f)
	((= (car triple) (caddr triple)) #f)
	((= (cadr triple) (caddr triple)) #f)
	(else #t)))

(define (make-unique-triple n)
  (filter unique-triple?
	  (flatmap (lambda (a) a)
		   (flatmap (lambda (i)
			      (map (lambda (j)
				     (map (lambda (k)
					    (list i j k))
					  (enumerate-interval 1 n)))
				   (enumerate-interval 1 n)))
			    (enumerate-interval 1 n)))))

;; 연습문제 2.42
;; -_-??
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter (lambda (positions) (safe? k positions))
		(flatmap
		 (lambda (rest-of-queens)
		   (map (lambda (new-row)
			  (adjoin-position new-row k rest-of-queens))
			(enumerate-interval 1 board-size)))
		 (queen-cols (- k 1))))))
  (queen-cols board-size))


;; 연습문제 2.43 
;; -_-??




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 그림언어

(define (right-split painter n)
  (if (= n 0) painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0) painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))




;;; 연습문제 2.44
(define (up-split painter n)
  (if (= n 0)
      (let ((smaller (up-split painer (- n 1))))
	(below (beside smaller smaller) painter))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-paris painter)
  (let ((combine4 (square-of-four identity filp-vert
				  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


;;; 연습문제 2.45
(define (split identity-op smaller-op)
  (define (rec-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec-split painter (- n 1))))
          (identity-op painter (smaller-op smaller smaller)))))
  rec-split)




;;; 그림틀
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

((frame-coord-map a-frame) (make-vect 0 0))
(origin-frame a-frame)


;;; 연습문제 2.46

(define (make-vect x y)
  (list x y))


(define (xcor-vect v)
  (car v))
 
(define (ycor-vect v)
  (cadr v))


(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))
 
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
 
(define (sub-vect v1 v2)
  (add-vect v1
            (scale-vect -1 v2)))

;;; 연습문제 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define s (make-frame (make-vect 0 0) (make-vect 4 4) (make-vect 2 8)))

(define (origin-frame frame)
  (car frame))

(origin-frame s)

(define (edge1-frame frame)
  (cadr frame))

(edge1-frame s)

(define (edge2-frame frame)
  (caddr frame))

(edge2-frame s)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define s (make-frame (make-vect 0 0) (make-vect 4 4) (make-vect 2 8)))

(define (origin-frame frame)
  (car frame))

(origin-frame s)

(define (edge1-frame frame)
  (cadr frame))

(edge1-frame s)

(define (edge2-frame frame)
  (cddr frame))

(edge2-frame s)



(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))



;;; 연습문제 2.48

(define (make-segment v1 v2)
  (make-vect v1 v2))

(define (start-segment segment)
  (xcor-vect segment))

(define (end-segment segment)
  (ycor-vect segment))


;;; 연습문제 2.49

(define (draw-outline frame)
  ((segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
			    (make-segment (make-vect 1 0) (make-vect 1 1))
			    (make-segment (make-vect 1 1) (make-vect 0 1))
			    (make-segment (make-vect 0 1) (make-vect 0 0))))
   frame))

(define (draw-xline frame)
  ((segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
			    (make-segment (make-vect 0 1) (make-vect 1 0))))
   frame))

(define (draw-diamond frame)
  ((segments->painter (list (make-segment (make-vect .5 0) (make-vect 1 .5))
			    (make-segment (make-vect 1 .5) (make-vect .5 1))
			    (make-segment (make-vect .5 1) (make-vect 0 .5))
			    (make-segment (make-vect 0 .5) (make-vect .5 0))))
   frame))


;;; 요건 나중에..
(define (wave frame)



  )





(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vect painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
		     (make-vect .5 .5)
		     (make-vect 1.0 .5)
		     (make-vect .5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect  0.0 0.0)
		     (make-vect .64 .35)
		     (make-vect .35 .65)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))


;;; 연습문제 2.50
(define (filp-horiz painter)
  ((transform-painter (make-vect 1.0 0.0)
		      (make-vect 0.0 0.0)
		      (make-vect 1.0 1.0))
   painter))

(define (ratate180 painter)
  ((transform-painter (make-vect 1.0 0.0)
		      (make-vect 0.0 0.0)
		      (make-vect 1.0 1.0))
   painter))

(define (rotate270 painter)
  ((transform-painter (make-vect 0.0 1.0)
		      (make-vect 0.0 0.0)
		      (make-vect 1.0 1.0))
   painter))

;;; 연습문제 2.51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           ((transform-painter (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)
            painter1))
          (paint-top
           ((transform-painter split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))
            painter2)))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))



;;; 연습문제 2.52