;; 2.5 Systems with Generic Operations

;; 2.5.1 Generic Arithmetic Operation

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  ;; ;; >> addition 2.79 2.80
  ;; (put 'equ? '(scheme-number scheme-number) ; 2.79
  ;;      (lambda (x y) (= x y)))
  ;; (put '=zero? '(scheme-number)
  ;;      (lambda (x) (= x 0))) ; 2.80
  ;; ;; << addition

  ;; ;; >> addition 2.81
  ;; (put 'exp '(scheme-number scheme-number)
  ;;      (lambda (x y) (tag (expt x y))))
  ;; ;; << addition 2.81
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; ;; >> addition 2.79 2.80
  ;; ;; (define (equ?-rat x y)
  ;; ;;   (let ((x_norm (make-rat (numer x) (denom x)))
  ;; ;;         (y_norm (make-rat (numer y) (denom y))))
  ;; ;;     (and (= (numer x_norm) (numer y_norm))
  ;; ;;          (= (denom x_norm) (denom y_norm)))))    
  ;; (define (equ?-rat x y)
  ;;   (= 0 (numer (sub-rat x y))))

  ;; (define (=zero?-rat x)
  ;;   (= 0 (numer x)))
  ;; ;; << addition 2.79 2.80

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  
  ;; >> addition 2.79 2.80
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
;;  (put '=zero? '(rational)
  (put '=zero? 'rational
       (lambda (x) (=zero?-rat x)))
  ;; << addition
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; ;; >> addition 2.79 2.80
  ;; (define (equ?-complex z1 z2)
  ;;   (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))
  ;; (define (=zero? z)
  ;;   (and (= 0 (real-part z)) (= 0 (imag-part z))))
  ;; ;; << addition 2.79 2.80
  
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

;; ;; >> addition 2.77
;;   (put 'real-part '(complex) real-part)
;;   (put 'imag-part '(complex) imag-part)
;;   (put 'magnitude '(complex) magnitude)
;;   (put 'angle '(complex) angle)
;; ;; << addition 2.77

;; ;; >> addition 2.79 2.80
;;   (put 'equ? '(complex complex)
;;        (lambda (z1 z2) (equ?-complex z1 z2)))
;;   (put '=zero? '(complex)
;;        (lambda (z) (=zero? z)))
;; ;; << addition 2.79 2.80
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
  
;; ex 2.77
(define *dispatch-table* (make-hash))

(define (put op type action)
  (hash-set! *dispatch-table* (cons op type) action))

(define (get op type)
  (hash-ref *dispatch-table* (cons op type) #f))

;;(install-scheme-number-package)
;;(install-rational-package)
;;(install-complex-package)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENETIC"
           (list op type-tags))))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))


;;(define z (make-complex-from-real-imag 2 5))

(define (square x)
  (* x x))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)


(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* magnitude z) (con (angle z)))
  (define (imag-part z)
    (* magnitude z) (sin (angle z)))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-all)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-polar-package)
  (install-rectangular-package)
'done)

(install-all)
;; <== run until this line

(define z (make-complex-from-real-imag 6 8))

(magnitude z)
(angle z)
(real-part z)
(imag-part z)

;; refer to likerivers12.scm
;; twice

;; ex 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((number? contents) datum)
        ((pair? datum) (cdr datum))
        (error "Bad tagged datum -- CONTENTS" datum)))


;; 
(= (+ 2 5) (add 2 5))
(= (- 10 7) (sub 10 7))
(= (* 10 2) (mul 10 2))
(= (/ 10 2) (div 10 2))

;; ex 2. 79
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define (equ? x y) (apply-generic 'equ? x y)) ; 2.79
(equ? 1 1)
(equ? 1 2)
(equ? (make-rational 2 3) (make-rational 2 3))
(equ? (make-rational 2 3) (make-rational 4 6))
(equ? (make-rational 2 3) (make-rational 4 5))
(equ? (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 2 3))
(equ? (make-complex-from-mag-ang 2 3) (make-complex-from-mag-ang 2 3))

;; ex 2. 80
(define (=zero? x) (apply-generic '=zero? x)) ; 2.80
(=zero? 0)
(=zero? 1)
(=zero? (make-rational 0 1))
(=zero? (make-rational 1 1))
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-complex-from-real-imag 1 0))

;; 2.5.2 Combining Data of Different Types

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))
                        
(add 1 (make-rational 1 2))

;; ex 2.81
(define *coercion-table* (make-hash))

(define (put-coercion from-type to-type proc)
  (hash-set! *coercion-table* (cons from-type to-type) proc))

(define (get-coercion from-type to-type)
  (hash-ref *coercion-table* (cons from-type to-type) #f))

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex
              complex->complex)


(add 1 2)
(add (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 3 4))

;; a.
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(define (exp x y) (apply-generic 'exp x y))
(exp 2 3)
(exp (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))

;;> hash-ref: no value found for key: (exp complex complex)

;; b.
;; let's change.

;; c.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2) (display "No method")
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags))))))))

(exp 2 3)
(exp (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))

;; ex 2.82
(define (apply-generic op . args)
  (define (coercable-into? types target-type)
    (andmap
     (lambda (type)
       (or (equal? type target-type)
           (get-coercion type target-type)))
       types))
  (define (find-coercion-type types)
    (ormap
     (lambda (target-type)
       (if (coercable-into? types target-type)
           target-type
           #f))
     types))
  (define (coerce-all args target-type)
    (map
     (lambda (arg)
       (let ((arg-type (type-tag arg)))
         (if (equal? arg-type target-type)
             arg
             ((get-coercion arg-type target-type) arg))))
     args))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((target-type (find-coercion-type type-tags)))
            (if target-type
                (apply
                 apply-generic
                 (append
                  (list op)
                  (coerce-all args target-type)))
                (error "No method for these types"
                       (list op type-tags))))))))

(install-scheme-number-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)

(add 1 (make-complex-from-real-imag 1 2))
(add 1 (make-rational 1 2))

;; ex 2.83
(define (integer->rational n)
  (make-rational n 1))

(put 'raise '(integer)
     (lambda (n) (integer-rational n)))

(define (rational->real r)
  (make-real
   (exact->inexact
    (/ (numer r) (denom r)))))

(put 'raise '(rational)
     (lambda (r) (rational->real r))

(define (real->complex r)
  (make-complex-from-real-imag r 0))

(put 'raise '(real)
     (lambda (r) (real->complex r)))

(define (raise x)
  (apply-generic 'raise x))

;; ex 2.84
(define (apply-generic op . args)
  (define (raise-into child parent)
    (let ((child-type (type-tag child))
          (parent-type (type-tag parent)))
      (cond
       ((equal? child-type parent-type) child-type)
       ((get 'raise (list child-type))
        (raise-into ((get 'raise (list child-type)) (contents s)) t))
       (error "No method to raise"))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contentns args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (cond
                 ((raise-into a1 a2)
                  (apply-generic op (raise-into a1 a2) a2))
                 ((raise-into a2 a1)
                  (apply-generic op a2 (raise-into a2 a1)))
                 (error "No method for these types"
                        (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))

(add 1 (make-rational 1 2))

;; ex 2.85
(put 'project '(rational)
    (lambda (r)
      (make-scheme-number
        (floor (/ (numer r) (denom r))))))

(put 'project '(real)
     (lambda (r)
       (let ((scheme-rat
              (rationalize
               (inexact->exact r) 1/100)))
         (make-rational
          (numerator scheme-rat)
          (denominator scheme-rat)))))

(put 'project '(complex)
     (lambda (c) (make-real (real-part c))))

(define (drop num)
  (let ((project-proc
         (get 'project (list (type-tag num)))))
    (if project-proc
        (let ((dropped (project-proc (contents num))))
          (if (equ? num (raise dropped))
              (drop dropped)
              num))
        num)))
(define (apply-generic op . args)
  (define (raise-into s t)  ;; try to raise 
    (let ((s-type (type-tag s))
          (t-type (type-tag t)))
      (cond
       ((equal? s-type t-type) s)
       ((get 'raise (list s-type))
        (raise-into ((get 'raise (list s-type)) (contents s)) t))
       (t #f))))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((o1 (car args))
                    (o2 (cadr args)))
                (cond
                 ((raise-into o1 o2)
                  (apply-generic-r op (raise-into o1 o2) o2))
                 ((raise-into o2 o1)
                  (apply-generic-r op o2 (raise-into o2 o1)))
                 (error "No method for these types"
                        (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))
      
      