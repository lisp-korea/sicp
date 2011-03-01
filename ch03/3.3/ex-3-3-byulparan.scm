
;; racket -l r5rs/run

;; 연습문제 3.12

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)

(define w (append! x y))
w
(cdr x)


;; 연습문제 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; what happen?
(last-pair z) 				


;; 연습문제 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x) y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))


(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1
(set-to-wow! z1)
z2
(set-to-wow! z2)

;; 연습문제 3.15

;; 연습문제 3.16

(define (count-pairs x)
  (if (not (pair? x)) 0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x)) 1)))

;; (define (count-pairs x)
;;   (if (not (pair? x)) (begin
;; 			(display x) (display "\n")
;; 			0)
;;       (+ (count-pairs (car x))
;; 	 (count-pairs (cdr x)) 1)))

;; car 과 cdr 이 같은 pair 를 가리키고 있다면?!

;; 세계의 pair 가 있지만..4를 출력.
(let ((a (list 'a)))
  (let ((b (list a a)))
    (count-pairs b)))

;; 7
(let ((a (cons 10 20)))
  (let ((b (cons a a)))
    (let ((c (cons b b)))
      (count-pairs c))))

;; 영원히 도는 프로시져
(let ((a (list 1 2)))
  (let ((b (cons 10 a)))
    (set-cdr! (cdr a) b)
    (count-pairs a)))

;; 연습문제 3.17

(define (find-item item seq)
  (cond ((null? seq) #f)
	((eq? item (car seq)) #t)
	(else (find-item item (cdr seq)))))

;; (let ((a '(1 2 3)))
;;   (let ((b (list 10 20 a 30)))
;;     (find-item a b)))

(define (new-count-pairs x)
  (let ((repo '()))
    (define (count-pairs x)
      (if (or (not (pair? x)) (find-item x repo)) 0
	  (begin
	    (set! repo (cons x repo))
	    (+ (count-pairs (car x))
	       (count-pairs (cdr x)) 1))))
    (count-pairs x)))



(let ((a (list 'a)))
  (let ((b (list a a)))
    (new-count-pairs b)))


(let ((a (cons 10 20)))
  (let ((b (cons a a)))
    (let ((c (cons b b)))
      (new-count-pairs c))))

(let ((a (list 1 2)))
  (let ((b (cons 10 a)))
    (set-cdr! (cdr a) b)
    (new-count-pairs a)))


;; 연습문제 3.18







