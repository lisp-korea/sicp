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
