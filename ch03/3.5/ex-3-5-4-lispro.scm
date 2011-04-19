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

; exercise 3.55
(define (partial-sums S)
  (cons-stream (stream-car S) (add-streams (stream-cdr S)
                                           (partial-sums S)))) 

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
               (newline)
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

; 3.5.3 Exploiting the Stream Paradigm

; exercise 3.63
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; exercise 3.64
(define (stream-limit stm tolerance)
  (let ((element1 (stream-car stm)) ; 앞의 원소
        (element2 (stream-car (stream-cdr stm)))) ; 뒤의 원소
    (if (< (abs (- element1 element2)) tolerance)
        element2
        (stream-limit (stream-cdr stm) tolerance))))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; exercise 3.65
(define ln2-stream
  (partial-sums (ln2-summands 1)))
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (square x) (* x x))

;; check time
;(define (timed-test proc)
;  (start-test proc (current-milliseconds)))
;
;(define (start-test proc start-time)
;  (let ((result (proc)))
;    (if result
;        (report-timed (- (current-milliseconds) start-time) result)
;        (= 0 1))))
;
;(define (report-timed elapsed-time result) 
;  (display "time : ")
;  (display elapsed-time)
;  (display ",  result : ")
;  (display result)
;  (newline))

; execute
(define (f)
  (let ((start-time 0))
    (begin
      (display "value : ")
      (set! start-time (current-milliseconds))
      (display (stream-limit (accelerated-sequence euler-transform ln2-stream) 0.001))
      (display "  time : ")
      (display (- (current-milliseconds) start-time))
      (newline)
      
      (display "value : ")
      (set! start-time (current-milliseconds))
      (display (stream-limit (euler-transform ln2-stream) 0.001))
      (display "  time : ")
      (display (- (current-milliseconds) start-time))
      (newline)
      
      (display "value : ")
      (set! start-time (current-milliseconds))
      (display (stream-limit ln2-stream 0.001))
      (display "  time : ")
      (display (- (current-milliseconds) start-time))
      (newline))))
(f)



(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

; exercise 3.66
(define a (pairs integers integers))

; exercise 1.16
(define (i-fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
            ((even? n) (fast-expt-iter a (square b) (/ n 2)))
            (else (fast-expt-iter (* a b) b (- n 1)))))

(define (even? n)
  (= (remainder n 2) 0))
  
; execute
(print-stream-n a 100) ; (1, 1) ~ (1, 51)
(newline)
(display (stream-ref a 101)) ; (1, 52)
(newline) (newline)
(display (- (* (i-fast-expt 2 0) (+ (* 2 (- 100 1)) 1)) 2)) ; (1, 100)
(newline)
(display (stream-ref a (- (* (i-fast-expt 2 1) (- 100 1 -1)) 3))) ; (1, 100)
(newline) (newline)
(display (- (* (i-fast-expt 2 98) (+ (* 2 (- 100 99)) 1)) 2)) ; (99, 100)
;(newline)
;(display (stream-ref a (- (* (i-fast-expt 2 98) (+ (* 2 (- 100 99)) 1)) 2))) ; (99, 100)
(newline) (newline)
(display (- (i-fast-expt 2 100) 2)) ; (100, 100)
;(newline)
;(display (stream-ref a (- (i-fast-expt 2 100) 2))) ; (100, 100)

(newline) (newline)
(display (- (* (i-fast-expt 2 8) (+ (* 2 (- 10 9)) 1)) 2)) ; (9, 10)
(newline)
(display (stream-ref a (- (* (i-fast-expt 2 8) (+ (* 2 (- 10 9)) 1)) 2))) ; (99, 100)
(newline) (newline)
(display (- (i-fast-expt 2 10) 2)) ; (10, 10)
(newline)
(display (stream-ref a (- (i-fast-expt 2 10) 2))) ; (20, 20)

; exercise 3.68
(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

; execute
(define a (pairs integers integers))
(print-stream-n a 100)
; exercise 1.16
(define (i-fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))

(define (even? n)
  (= (remainder n 2) 0))

; exercise 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean_triples
  (stream-filter (lambda (x)
                   (= (+ (square (car x)) (square (cadr x))) (square (caddr x))))
                 (triples integers integers integers)))

; execute
(define b (triples integers integers integers))
(print-stream-n b 10)
(print-stream-n pythagorean_triples 5)

; exercise 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((weight s1car s2car)
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight) weight)))

; execute
(define a (weighted-pairs integers
                          integers
                          (lambda (x y)
                            (let ((xi (car x))
                                  (xj (cadr x))
                                  (yi (car y))
                                  (yj (cadr y)))
                              (< (+ xi xj) (+ yi yj))))))
(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))
(define b (weighted-pairs S
                          S
                          (lambda (x y)
                            (let ((xi (car x))
                                  (xj (cadr x))
                                  (yi (car y))
                                  (yj (cadr y)))
                              (< (+ (* 2 xi) (* 3 xj) (* 5 xi xj))
                                 (+ (* 2 yi) (* 3 yj) (* 5 yi yj)))))))

(print-stream-n a 50)
(newline) (newline)
(print-stream-n b 50)
; exercise 3.71
(define (cube x) (* x x x))

(define (Ramanujan-stream-filter stream)
  (let ((x (stream-car stream))
        (y (stream-car (stream-cdr stream))))
    (cond ((stream-null? stream) the-empty-stream)
          ((= x y)
           (cons-stream (stream-car stream)
                        (Ramanujan-stream-filter (stream-cdr stream))))
          (else (Ramanujan-stream-filter (stream-cdr stream))))))

(define Ramanujan
  (Ramanujan-stream-filter
   (stream-map (lambda (x)
                 (let ((i (car x))
                       (j (cadr x)))
                   (+ (cube i) (cube j))))
               (weighted-pairs integers
                               integers
                               (lambda (x y)
                                 (let ((xi (car x))
                                       (xj (cadr x))
                                       (yi (car y))
                                       (yj (cadr y)))
                                   (< (+ (cube xi) (cube xj))
                                      (+ (cube yi) (cube yj)))))))))

(define (Ramanujan-list-stream-filter stream)
  (let ((x (stream-car stream))
        (y (stream-car (stream-cdr stream))))
    (let ((x_cube_sum (+ (cube (car x)) (cube (cadr x))))
          (y_cube_sum (+ (cube (car y)) (cube (cadr y)))))
      (cond ((stream-null? stream) the-empty-stream)
            ((= x_cube_sum y_cube_sum)
             (cons-stream (list x_cube_sum x y)
                          (Ramanujan-list-stream-filter (stream-cdr stream))))
            (else (Ramanujan-list-stream-filter (stream-cdr stream)))))))

(define Ramanujan-list
  (Ramanujan-list-stream-filter
   (weighted-pairs integers
                   integers
                   (lambda (x y)
                     (let ((xi (car x))
                           (xj (cadr x))
                           (yi (car y))
                           (yj (cadr y)))
                       (< (+ (cube xi) (cube xj))
                          (+ (cube yi) (cube yj))))))))

; execute
(print-stream-n Ramanujan 6 6)
(newline)
(print-stream-n Ramanujan-list 6 1)
; exercise 3.72
(define (cube x) (* x x x))

(define (Three-square-stream-filter stream)
  (let ((x (stream-car stream))
        (y (stream-car (stream-cdr stream)))
        (z (stream-car (stream-cdr (stream-cdr stream)))))
    (let ((x_square_sum (+ (square (car x)) (square (cadr x))))
          (y_square_sum (+ (square (car y)) (square (cadr y))))
          (z_square_sum (+ (square (car z)) (square (cadr z)))))
      (cond ((stream-null? stream) the-empty-stream)
            ((= x_square_sum y_square_sum z_square_sum)
             (cons-stream (list x_square_sum x y z)
                          (Three-square-stream-filter (stream-cdr stream))))
            (else (Three-square-stream-filter (stream-cdr stream)))))))

(define Three-square-number
  (Three-square-stream-filter
   (weighted-pairs integers
                   integers
                   (lambda (x y)
                     (let ((xi (car x))
                           (xj (cadr x))
                           (yi (car y))
                           (yj (cadr y)))
                       (< (+ (square xi) (square xj))
                          (+ (square yi) (square yj))))))))

; execute
(print-stream-n Three-square-number 10 1)
; exercise 3.74
;(define (make-zero-crossings input-stream last-value)
;  (cons-stream
;   (sign-change-detector (stream-car input-stream) last-value)
;   (make-zero-crossings (stream-cdr input-stream)
;                        (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

; make the procedure and stream
(define (sign-change-detector b a)
  (let ((ab (* a b)))
    (cond ((< ab 0)
           (if (< a 0)
               1
               -1))
          ((= ab 0) ; 0은 양수로 취급.
           (if (= a 0)
               (sign-change-detector b 1)
               (sign-change-detector 1 a)))
          (else 0)))) ; ab > 0

(define sense-data
  (cons-stream 1 (cons-stream 2 (cons-stream 1.5 (cons-stream 1 (cons-stream 0.5 (cons-stream -0.1 (cons-stream -2 (cons-stream -3 (cons-stream -2 (cons-stream -0.5 (cons-stream 0.2 (cons-stream 3 (cons-stream 4 sense-data))))))))))))))

; exercise 3.75
;(define (make-zero-crossings input-stream last-value last-avpt)
;  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;    (cons-stream (sign-change-detector avpt last-avpt)
;                 (make-zero-crossings (stream-cdr input-stream)
;                                      (stream-car input-stream)
;                                      avpt))))

; exercise 3.76
(define (smooth stm)
  (let ((a (stream-car stm))
        (b (stream-car (stream-cdr stm))))
    (cons-stream (/ (+ a b) 2)
                 (smooth (stream-cdr stm)))))

(define (make-zero-crossings input-stream last-value)
  (define (zero-crossing_s smooths)
    (let ((last-avpt (stream-car smooths))
          (avpt (stream-car (stream-cdr smooths))))
      (cons-stream (sign-change-detector avpt last-avpt)
                   (zero-crossing_s (stream-cdr smooths)))))
  (zero-crossing_s (cons-stream last-value
                                (smooth input-stream))))

; execute
(print-stream-n zero-crossings 13 13)
