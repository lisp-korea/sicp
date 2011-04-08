
;; 3.5.2 무한 스트림

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define the-empty-stream '())
(define (stream-null? s)
  (eq? s the-empty-stream))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))


(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))


(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
	(apply proc (map stream-car argstreams))
	(apply stream-map
	       (cons proc (map stream-cdr argstreams))))))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))




(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y)
  (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7))) integers))


(define (fibogen a b)
  (cons-stream a (fibogen b (+ a b))))

(define fibs (fibogen 0 1))


;; 에라토스테네스의 체
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter (lambda (x)
			   (not (divisible? x (stream-car stream))))
			 (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))


;; 스트림을 드러나지 않게 정의하는 방법
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0 (cons-stream 1 (add-stream (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
	  ((divisible? n (stream-car ps)) false)
	  (else (iter (stream-cdr ps)))))
  (iter primes))

