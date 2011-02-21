;; 3.1.1 Local Satate Variables

(def balance (atom 100))
(defn withdraw [amount]
  (if (>= @balance amount)
    (do
      (reset! balance (- @balance amount))
      @balance)
    "Insufficient funds"))

(withdraw 25)
75

(withdraw 25)
50

(withdraw 60)
"Insufficient funds"

(withdraw 15)
35

(def new-withdraw
     (let [balance (atom 100)]
       (fn [amount]
	 (if (>= @balance amount)
	   (do
	     (reset! balance (- @balance amount))
	     @balance)
	   "Insufficient funds"))))

(defn make-withdraw [balance]
  (let [b (atom balance)]
    (fn [amount]
      (if (>= @b amount)
	(do
	  (reset! b (- @b amount))
	  @b)
	"Insufficient funds"))))

(def W1 (make-withdraw 100))
(def W2 (make-withdraw 100))

(W1 50)
50

(W2 70)
30

(W2 40)
"Insufficient funds"

(W1 40)
10

(defn make-account [balance]
  (let [b (atom balance)]
    (defn withdraw [amount]
      (if (>= @b amount)
	(do
	  (reset! b (- @b amount))
	  @b)
	"Insufficient funds"))
    (defn deposit [amount]
      (reset! b (+ @b amount))
      @b)
    (defn dispatch [m]
      (cond (= m 'withdraw) withdraw
	    (= m 'deposit) deposit
	    :else (throw (Exception. (str "Unknown request -- MAKE-ACCOUNT" m))))))
  dispatch)

(def acc (make-account 100))

((acc 'withdraw) 50)
50

((acc 'withdraw) 60)
"Insufficient funds"

((acc 'deposit) 40)
90

((acc 'withdraw) 60)
30

(def acc2 (make-account 100))

;; ex 3.1

(defn make-accumulator [n]
  (let [acc (atom n)]
    (fn [x]
      (do
	(reset! acc (+ @acc x))
	@acc))))

(def A (make-accumulator 5))

(A 10)

(A 10)

;; ex 3.2

(defn make-monitored [f]
  (let [count (atom 0)]
    (fn [& args]
      (if (= (first args) 'how-many-calls?) @count
	  (do
	    (reset! count (+ @count 1))
	    (apply f args))))))

(defn sqrt [n]
  (Math/sqrt n))
(def s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)
1

;; ex 3.3
(defn make-account [balance password]
  (let [b (atom balance)
	pwd password]

    (defn withdraw [amount]
      (if (>= @b amount)
	(do
	  (reset! b (- @b amount))
	  @b)
	"Insufficient funds"))
    (defn deposit [amount]
      (reset! b (+ @b amount))
      @b)
    (defn dispatch [p m]
      (cond
       (not (= p pwd)) (throw (Exception. "Incorrect password"))
       (= m 'withdraw) withdraw
       (= m 'deposit) deposit
       :else (throw (Exception. (str "Unknown request -- MAKE-ACCOUNT" m)))))))
(def acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)

;; ex 3.4
(defn make-account [balance password]
  (let [b (atom balance)
	pwd password
	failure (atom 0)]

    (defn withdraw [amount]
      (if (>= @b amount)
	(do
	  (reset! b (- @b amount))
	  @b)
	"Insufficient funds"))
    (defn deposit [amount]
      (reset! b (+ @b amount))
      @b)
    (defn call-the-cops []
      (throw (Exception. "call the cops!")))
    (defn dispatch [p m]
      (cond
       (not (= p pwd)) (if (< @failure 6)
			 (do (reset! failure (+ @failure 1))
			     (throw (Exception. "Incorrect password")))
			 (call-the-cops))
       (= m 'withdraw) withdraw
       (= m 'deposit) deposit
       :else (throw (Exception. (str "Unknown request -- MAKE-ACCOUNT" m)))))))

(def acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)

;; 3.2 The Benefits of Introducing Assignment

(def random-init (atom 0))
(defn rand-update [n])
(defn gcd [a b]
  (loop [x a
	 y b]
    (if (= y 0)
      x
      (recur y (mod x y)))))
  
(def random
     (let [x random-init]
       (fn []
	 (reset! x (rand-update @x))
	 @x)))

(defn cesaro-test []
  (= (gcd (random) (random)) 1))
(defn estimate-pi [trials]
  (sqrt (/ 6 (monte-carlo tirals cesaro-test))))
(defn monte-carlo [trials experiment]
  (loop [trials-remaining trials
	 trials-passed 0]
    (cond (= trials-remaining 0) (/ trials-passed trials)
	  (experiment) (recur (- trials-remaining 1) (+ trials-passed 1))
	  :else (recur (- trials-remaining 1) trials-passed))))


;; without assignemnt...

(defn random-gcd-test [trials initial-x]
  (loop [trials-remaining trials
	 trials-passed 0
	 x initial-x]
    (let [x1 (rand-update x)]
      (let [x2 (rand-update x1)]
	(cond (= trials-remaining 0) (/ trials-passed trials)
	      (= (gcd x1 x2) 1) (recur (- trials-remaining 0)
				       (+ trials-passed 1)
				       x2)
	      :else (recur (- trials-remaining 1)
			   trials-passed
			   x2))))))
(defn estimate-pi [trials]
  (sqrt (/ 6 (random-gcd-test trials random-init))))


;; ex 3.5

(defn random-in-range [low high]
  (let [range (- high low)]
    (+ low (Math/round (rand range)))))

(defn estimate-integral [p x1 x2 y1 y2 trials]
  (* (monte-carlo trials (fn []
			   (let [rx (random-in-range x1 x2)
				 ry (random-in-range y1 y2)]
			     (< ry (p rx))))
		  (* (- x2 x1) (- y2 y1)))))

;; ex 3.6

(def random-init (atom 0))
(defn rand-update [n]
  (mod (+ (* n 3) 5) 19))
(def rand-new
     (let [x random-init]
       (fn [action]
	 (cond (= action 'generate) (do (reset! x (rand-update @x)) @x)
	       (= action 'reset) (fn [n] (reset! x n))))))

