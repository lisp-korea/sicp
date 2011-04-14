; stream
(define true (= 0 0))
(define false (= 1 0))

(define (cons-stream a b)
  (cons a (delay b)))
(define the-empty-stream '()) 
(define stream-null? null?) 
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

; section 3.5
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;;;SECTION 3.5.2
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

; exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

; exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

; print-stream-n
(define (print-stream-n S n)
  (define (iter i)
    (if (= i n)
        'done
        (begin (display (stream-ref S i))
               (display "  ")
               (if (= (remainder (+ i 1) 10) 0)
                   (newline))
               (iter (+ i 1)))))
  (iter 0))

; exercise 3.59
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series coff_stm)
  (let ((integrate_s (cons-stream 1 ; 상수 c
                                  (div-streams coff_stm integers))))
    (stream-cdr integrate_s)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 1
               (integrate-series (scale-stream sine-series -1))))
(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

; exercise 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (mul-series (stream-cdr s1) s2)
                            (scale-stream (stream-cdr s2) (stream-car s1)))))
; exercise 3.61
(define (invert-unit-series S)
  (define X
    (cons-stream 1
                 (scale-stream (mul-series (stream-cdr S) X) -1)))
  X)

; exercise 3.62
; s1 / s2
(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "Divide by zero" s2)
      (mul-series s1 (invert-unit-series s2))))

; execute
(define f1 (div-series sine-series cosine-series))
(print-stream-n cosine-series 10)
(print-stream-n sine-series 10)
(print-stream-n f1 10)
(newline) (newline)

(define constant_one
  (add-streams (mul-series sine-series sine-series)
               (mul-series cosine-series cosine-series)))
(define one_minus_x
  (cons-stream 1
               (cons-stream -1
                            (add-streams ones
                                       (scale-stream ones -1)))))
(define f2 (div-series constant_one one_minus_x))
(print-stream-n constant_one 10)
(print-stream-n one_minus_x 10)
(print-stream-n f2 10)

;;
