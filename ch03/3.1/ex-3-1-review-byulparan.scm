
;; 3. 모듈, 물체, 상태

;; 1,2 장에서는 기본 프로시져 기본 데이터를 묶어서 어떻게 더 복잡한 물체를 만드는지
;; 살펴보고 크고 복잡한 시스템을 설계할때 꼭 드러낼 것만 추려내고 속 내용을 감추는 일이
;; 얼마나 중요한 구실을 하는지 살펴보았다.


;; 3.1 덮어쓰기와 갇힌 상태(local state)

;; 3.1.1 갇힌 상태 변수

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin
	(set! balance (- balance amount))
	balance)
      "Insfficient funds"))

;; 위의 balance 는 전역변수. 맨 바깥 환경에 정의되었기 때문에 모든
;; 프로세스가 마음대로 값을 읽고 쓸 수 있다.


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insfficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50)
(W2 70)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insfficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request --MAKE-ACOUNT" m))))
  dispatch)


(define acc (make-account 100))
((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)


;; 3.1.2 덮어쓰기가 있어서 좋은 점



;; 덮어쓰기를 이용하여 푼 몬테카를로
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



;; 몬테카를로 문제를 덮어쓰기 없이 풀 경우
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
	(cond ((= trials-remaining 0) (/ trials-passed trials))
	      ((= (gcd x1 x2) 1)
	       (iter (- trials-remaining 1) (+ trials-passed 1) x2))
	      (else
	       (iter (- trials-remaining 1) trials-passed x2))))))
  (iter trials 0 initial-x))

;; rand-update 함수가 이전에 나온 random 값을 가지고 새로운 난수를 만들어내는데
;; 이전에 나온 난수값을 저장을 하지 않기때문에 estimate-pi 와 random-gcd-test 가
;; 난수값을 저장하고 유지하기 위해 이전 난수값을 인자로 전달하고 있다.-프로시저가 난수를 만드는 일에
;; 얽혀있다.




;; 3.1.3 덮어쓰기를 끌어들인 대가

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))
(W 20)
(W 10)


(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))
(D 20)
(D 10)

;; make-decrementer 는 맞바꿈 계산 방식에 따라 어떻게 돌아가는지 설명할 수 있다
((make-decrementer 25) 20)

((lambda (amount) (- 25 amount)) 20)

(- 25 20)

5

;; make-simplified-withdraw 를 맞바꿈 계산법에 따라 억지로 풀어내려 해보자
((make-simplified-withdraw 25) 20)

((lambda (amount) (set! balance (- 25 amount)) 25) 20)

(set! balance 5) 25

;; balance 에 5를 덯어썼으나 전체식은 25가 되었다라고 설명할 수 밖에 없다.
;; set! 전의 balance 와 뒤에 오는 balance 가 다르다고 설명할 수 있으나
;; 인자와 인자값을 맞바꾸어 계산하는 방법에서는 그렇게 하지 못한다


;; 더 심각한 문제!

(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))

;; D1 과 D2 는 계산하는 방식이 똑같기 때문에 같은 물체로 볼 수 있다.


(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))

(W1 20)
(W1 20)

(W2 20)

;; 어떤식에서 W1 을 W2 로 맞바꾸더면 계산 결과가 달라진다.





;; 
;; '같은 것을 같은 것으로 맞바꿀 수 있다' 는 원칙에 따라 식을 계산할때 식의 값이 달라지지 않는다는
;; 성질이 뒷받침된다면, 그런언어를 ' 뜻이 한결같은(referential trasparent)' 언어라고 한다
;;


;; 프로그램에서 잇달아 덮어쓰기를 할때에는, 덮어쓰는 차례가 뒤바뀌지 않게 무척조심해야 한다.
;; 덮어쓸때마다 변수값이 바뀌기 때문에, 올바른 차례로 변수 값을 덮어쓰는지 따져 보아야 한다.



	  

 
