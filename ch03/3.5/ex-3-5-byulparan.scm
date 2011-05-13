
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




;; ======================================================================
;; ======================================================================
;; 3.5.5 모듈로 바라본 함수와 물체




;; 정의
;; 몬테카를로 정의는 마구잡이 수를 테스트함수에 집어넣어 전체 검사 조건을
;; 만족하는 비율을 통해 원하는 값을 찾아가는 시뮬레이션이다.
;; 전체 실험 횟수중에 테스트가 참일 확률값을 바탕으로 어떤 결론을 이끌어 내는데
;; 마구잡이로 고른 두 수의 최대공약수가 1일 확률을 6/pi^2 라고 하면 몬테카를로
;; 방법을 쓰면 이사실에서 pi 값을 어림잡아 계산할 수 있다.

;; 3.1.2 의 덮어쓰기를 사용한 몬테카를로 시뮬레이션.
(define random-init 1)

(define (rand-update x)
  (modulo (* x 1664525) 1013904223))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))


;; 덮어쓰기 없이 스트림기법을 사용한 방식

(define random-numbers
  (cons-stream random-init
	       (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
			random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream) (next (+ passed 1) failed)
                                     (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
	      (monte-carlo cesaro-stream 0 0)))



;;; 연습문제 3.81

(define (random-numbers s-in)
  (define (action x m)
    (cond ((eq? m 'generate) (rand-update x))
          (else m)))
  (cons-stream
   random-init
   (stream-map action (random-numbers s-in) s-in)))

(define s0 (cons-stream 'generate s0))

(define s1
  (cons-stream 'generate
               (cons-stream 'generate
                            (cons-stream
                             367
                             (cons-stream
                              'generate (cons-stream random-init s0))))))

(define rs0 (random-numbers s0))

(define rs1 (random-numbers s1))

;;; 연습문제 3.82

(define (random-numbers-in-range low high init)
  (define random-max 12344)
  (define random-numbers
    (cons-stream init
                 (stream-map rand-update random-numbers)))
  (define (rand-update x)
    (let ((a (expt 2 32))
          (c 1103515245)
          (m 12345))
      (modulo (+ (* a x) c) m)))
  (let ((range (- high low)))
    (stream-map (lambda (x)
                  (+ low (* range (/ x random-max))))
                random-numbers)))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (estimate-integral P x1 x2 y1 y2)
  (define ex-stream
    (stream-map (lambda (x y) (P x y))
                (random-numbers-in-range x1 x2 788)
                (random-numbers-in-range y1 y2 2310)))
  (let ((area (* (- x2 x1) (- y2 y1))))
    (stream-map (lambda (frac)
                  (* frac area))
                (monte-carlo ex-stream 0 0))))
(define (square x)
  (* x x))

(define pi-stream
  (estimate-integral (lambda (x y)
                       (< (+ (square x) (square y)) 1.0))
                     -1.0 1.0 -1.0 1.0))


;;; 함수형 프로그래밍에서 시간의 문제

(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream)) (stream-cdr amount-stream))))

(define amount-stream
  (cons-stream 10 amount-stream))

(stream-ref (stream-withdraw 1000 amount-stream) 40)



