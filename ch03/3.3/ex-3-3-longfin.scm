;; 3.3.1 Mutable List Structure

(define (ncons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))


;; ex 3.12

(define (nappend x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
;; [[a] [+]
;;        +-[[b] [nil]]
(define y (list 'c 'd))
;; [[c] [+]
;;        +-[[d] [nil]]
(define z (nappend x y))
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [+]]
;;                                +-[[d] [nil]]

(cdr x)
;;(b)

(define w (append! x y))
;; (last-pair x)
;; [[b] [nil]]

;;(set-cdr! (last-pair x) y)
;; [[b] [+]]
;;        +=> y

;; x
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [+]]
;;                                +-[[d] [nil]]

(cdr x)
;; [[b] [+]]
;;        +-[[c] [+]]
;;                   +-[[d] [nil]]

;; ex 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; (list 'a 'b 'c) => x
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [nil]]

;; (set-cdr! (last-pair x) x)
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [+]]
;;                                +=> x

;; z is infinite sequence.

;; ex 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

;; (list 'a 'b 'c 'd) => x1
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [+]]
;;                               +-[[d] [nil]]

;; '() => y1
;; nil

;; (cdr x1) => temp1
;; [[b] [+]]
;;        +-[[c] [+]]
;;                   +-[[d] [nil]]

;; (set-cdr! x1 y1) => x1'
;; [[a] [+]]
;;        +-[[b] [+]]
;;                    +-[[c] [+]]
;;                               +-[[d] [nil]]

;; temp1 => x2
;; x1' => y2

;; (cdr x2) => temp2
;; [[c] [+]]
;;        +-[[d] [nil]]

;; (set-cdr! x2 y2) => x2'
;; [[b] [+]]
;;        +-[[c] [+]]
;;                   +-[[d] [+]]
;;                               +- y2

;; temp2 => x3
;; x2' => y3
;; ....

;; mystery reverses sequence.

;; v : (a b c d)
;; w : (d c b a)


