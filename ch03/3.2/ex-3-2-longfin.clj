;; 3.2 The Environment Model of Evaluation

;; 3.2.1 The Rules for Evaluation

(defn square [x]
  (* x x))

;; equals

(def square
     (fn [x] (* x x)))
"
                     +----------------------+
                     | other variables      |
          global --->|                      |
          env        | square: --+          |
                     +-----------|----------+
                                 |       ^
          (define (square x)     |       |
            (* x x))             V       |
                             .---.---.   |
                             | O | O-+---+
                             `-|-^---'
                               |
                               V
                             parameters: x
                             body: (* x x)
"


;; 3.2.2 Applying Simple Procedures

(defn sqaure [x]
  (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(defn f [a]
  (sum-of-squares (+ a 1) (* a 2)))

"
                    +--------------------------------------------+
                    | sum-of-squares:                            |
          global -->| square:                                    |
          env       | f: --+                                     |
                    +------|--------------+--------------+-------+
                           |     ^        |     ^        |     ^
                           |     |        |     |        |     |
                           V     |        V     |        V     |
                       .---.---. |    .---.---. |    .---.---. |
                       | O | O-+-+    | O | O-+-+    | O | O-+-+
                       `-|-^---'      `-|-^---'      `-|-^---'
                         |              |              |
                         V              V              V
             parameters: a          parameters: x  parameters: x, y
             body: (sum-of-squares  body: (* x x)  body: (+ (square x)
                     (+ a 1)                                (square y))
                     (* a 2))

			evaluating (f 5)

                    +-----------------------------------------------------+
          global -->|                                                     |
          env       +-----------------------------------------------------+
                      ^              ^                ^               ^
          (f 5)       |              |                |               |
                  +------+       +-------+        +------+        +-------+
            E1 -->| a: 5 |  E2 ->| x: 6  |  E3 -->| x: 6 |  E4 -->| x: 10 |
                  |      |       | y: 10 |        |      |        |       |
                  +------+       +-------+        +------+        +-------+
             (sum-of-squares   (+ (square x)       (* x x)         (* x x)
               (+ a 1)            (square u))
               (+ a 2))
"

;; ex 3.9

(defn factorial [n]
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(factorial 6)

;; E1 [n 6] (if (=n 1) 1(* n (factorial (- n 1))))
;; E2 [n 5] (if (=n 1) 1(* n (factorial (- n 1))))
;; E3 [n 4] (if (=n 1) 1(* n (factorial (- n 1))))
;; E4 [n 3] (if (=n 1) 1(* n (factorial (- n 1))))
;; E5 [n 2] (if (=n 1) 1(* n (factorial (- n 1))))
;; E6 [n 1] (if (=n 1) 1(* n (factorial (- n 1))))

;; E1 ~ E6 point global environment.

(defn factorial-iter [product counter max-count]
  (if (> counter max-count)
    product
    (factorial-iter
     (* counter product)
     (+ counter 1)
     max-count)))
(defn factorial [n]
  (factorial-iter 1 1 n))

(factorial 6)

;; E1 [n 6] (factorial-iter 1 1 n)
;; E2 [counter 1, product 1, max-count 6] (if ...)
;; E3 [counter 2, product 1, max-count 6] (if ...)
;; E4 [counter 3, product 2, max-count 6] (if ...)
;; E5 [counter 4, product 6, max-count 6] (if ...)
;; E6 [counter 5, product 24, max-count 6] (if ...)
;; E7 [counter 6, product 120, max-count 6] (if ...)
;; E8 [counter 7, product 720, max-count 6] (if ...)

;; E1 ~ E8 point global environment.


;; 3.2.3 Frames as the Repository of Local State

(defn make-withdraw [balance]
  (let [b (atom balance)]
    (fn [amount]
      (if (>= @b amount)
	(do
	  (reset! b (- @b amount))
	  @b)
	"Insufficient funds"))))

(def W1 (make-withdraw 100))

(W1 50)

"
                    +---------------------------+
          global -->| make-withdraw: --+        |
          env       +------------------|--------+
                                       |      ^
                                       V      |
                                   .---.---.  |
                                   | O | O-+--+
                                   `-|-^---'
                                     |
                                     V
                   parameters: balance
                   body: (lambda (amount)
                           (if (>= balance amount)
                               (begin (set! balance
                                            (- balance amount))
                                      balance)
                               \"Insufficient funds\"))
"

(def W1 (make-withdraw 100))

"
                    +-----------------------------------------------+
                    | make-withdraw: -----------------------+       |
          global -->|                                       |       |
                    | W1: --+                               |       |
                    +-------|-------------------------------|-------+
                            |                ^              |     ^
                            |                |              V     |
                            |        +-------+------+   .---.---. |
                            |  E1 -->| balance: 100 |   | O | O-+-+
                            |        +--------------+   `-|-^---'
                            V                ^            |
                        .---.---.            |            V
                      +-+-O | O-+------------+    parameters: balance
                      | `---^---'                 body: ...
                      V
              parameters: amount
              body: (if (>= balance amount)
                        (begin (set! balance (- balance amount))
                               balance)
                        \"Insufficient funds\")
"

(W1 50)

"

                    +---------------------------------------------------+
                    | make-withdraw: ...                                |
          global -->|                                                   |
          env       | W1: --+                                           |
                    +-------|-------------------------------------------+
                            |               ^
                            |               |
                            |       +-------+------+ Here is the balance
                            | E1 -->| balance: 100 | that will be changed
                            |       +--------------+ by the set!.
                            V               ^   ^
                        .---.---.           |   +----+
                        | O | O-+-----------+        |
                        `-|-^---'             +------+-----+
                          |                   | amount: 50 |
                          V                   +------------+
                parameters: amount   (if (>= balance amount)
                body: ...                (begin (set! balance
                                                      (- balance amount))
                                                balance)
                                         \"Insufficient funds\")




                     +------------------------------------+
                     | make-withdraw: ...                 |
          global --->|                                    |
          env        | W1: --+                            |
                     +-------|----------------------------+
                             |                   ^
                             |                   |
                             |            +------+------+
                             |     E1 --->| balance: 50 |
                             |            +-------------+
                             V                   ^
                         .---.---.               |
                         | O | O-+---------------+
                         `-|-^---'
                           |
                           V
                    parameters: amount
                    body: ...
"

(def W2 (make-withdraw 100))

"

                   +-------------------------------------------------+
                   | make-withdraw: ...                              |
          global ->| W2: ---------------------------+                |
          env      | W1: --+                        |                |
                   +-------|------------------------|----------------+
                           |              ^         |              ^
                           |              |         |              |
                           |       +------+------+  |       +------+-------+
                           |  E1 ->| balance: 50 |  |  E2 ->| balance: 100 |
                           |       +-------------+  |       +--------------+
                           V              ^         V              ^
                       .---.---.          |     .---.---.          |
                       | O | O-+----------+     | O | O-+----------+
                       `-|-^---'                `-|-^---'
                         | +----------------------+
                         V V
                  parameters: amount
                  body: ...
"

;; ex 3.10

(defn make-withdraw [initial-amount]
  (let [balance (atom initial-amount)]
    (fn [amount]
      (if (>= @balance amount)
	(do
	  (reset! balance (- @balance amount))
	  @balance)
	"Insufficient funds"))))

(defn make-withdraw [initial-amount]
  ((fn [balance]
     (fn [amount]
       (if (>= @balance amount)
	 (do
	   (reset! balance (- @balance amount))
	   @balance)
	 "Insufficient funds"))) (atom initial-amount)))

(def W1 (make-withdraw 100))
;; Global [make-withdraw : (fn)]
;; E1 [initial-amount 100]
;; E2 [balance 100]


;; 3.2.4 Internal Definitions

(defn average [a b]
  (int (/ (+ a b) 2)))
(defn sqrt [x]
  (defn good-enough? [guess]
    (< (Math/abs (- (square guess) x)) 0.001))
  (defn improve [guess]
    (average guess (/ x guess)))
  (defn sqrt-iter [guess]
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

"
                    +--------------------------------------------------+
          global -->| sqrt: --+                                        |
          env       |         |                                        |
                    +---------|----------------------------------------+
                              V       ^                   ^
                          .---.---.   |                   |
               +----------+-O | O-+---+        +----------+------------+
               |          `---^---'            | x: 2                  |
               V                         E1 -->| good-enough?: -+      |
          parameters: x                        | improve: ...   |      |
          body: (define good-enough? ...)      | sqrt-iter: ... |      |
                (define improve ...)           +----------------|------+
                (define sqrt-iter ...)          ^  ^            |     ^
                (sqrt-iter 1.0)                 |  |            V     |
                                      +---------++ |        .---.---. |
                                E2 -->| guess: 1 | |        | O | O-+-+
                                      +----------+ |        `-|-^---'
                                call to sqrt-iter  |          |
                                                   |          V
                                         +---------++    parameters: guess
                                   E3 -->| guess: 1 |    body: (< (abs ...)
                                         +----------+             ...)
                                   call to good-enough?
"

;; ex 3.11

(defn make-account [balance]
  (let [b (atom balance)]
    (defn withdraw [amount]
      (if (>= @b amount)
	(do
	  (reset! b (- @b amount))
	  @b))
      "Insufficient funds")
    (defn deposit [amount]
      (reset! b (+ @b amount))
      @b)
    (defn dispatch [m]
      (cond (= m 'withdraw) withdraw
	    (= m 'deposit) deposit
	    :else (throw (Exception. (str "Unknown request -- MAKE-ACCOUNT" m)))))
    dispatch))


;; global [make-account => parameter:x, body: ...]

(def acc (make-account 50))

;; E1 [balance: 100]
;; E2 [b: (atom 100), withdraw => parameter:amount, body:..., deposit => parameter:amount:amount, body:...]

((acc 'deposit) 40)

;; E3 [amount: 40] -> E2

((acc 'withdraw) 60)

;; E4 [amount: 60] -> E2

(def acc2 (make-account 100))

;; E1' [balance: 100]
;; E2' [b: (atom 100), withdraw => parameter:amount, body:..., deposit => parameter:amount:amount, body:...]


;; acc => E2
;; acc2 => E2'