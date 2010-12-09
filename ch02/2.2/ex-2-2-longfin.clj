(defn list-ref [items n]
  (if (= n 0)
    (first items)
    (list-ref (rest items) (dec n))))
(def squares (list 1 4 9 16))

(list-ref squares 3)

(defn length [items]
  (if (empty? items)
    0
    (inc (length (rest items)))))

(def odds (list 1 3 5 7))

(length odds)

(defn length [items]
  (loop [arr items
	 count 0]
    (if (empty? arr)
      count
      (recur (rest arr) (inc count)))))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

;; ex 2.17

(defn last-pair [items]
  (loop [arr items]
    (if (= (length arr) 1)
      (first arr)
      (recur (rest arr)))))

(last-pair (list 23 72 149 34))

;; ex 2.18

(defn reverse-recur [items]
  (if (= (length items) 1)
    items
    (append (reverse-recur (rest items)) (list (first items)))))
(reverse-recur (list 1 4 9))

(defn reverse-iter [items]
  (loop [arr items
	 reversed (list)
	 ]
    (if (= (length items) (length reversed))
      reversed
      (recur (rest arr) (append (list (first arr)) reversed)))))

(reverse-iter (list 3 4))

;; ex 2.19

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(defn no-more? [values]
  (empty? values))

(defn first-denomination [values]
  (first values))

(defn except-first-denomination [values]
  (rest values))

(defn cc [amount coin-values]
  (cond (= amount 0) 1
	(or (< amount 0) (no-more? coin-values)) 0
	:else
	(+ (cc amount
	       (except-first-denomination coin-values))
	   (cc (- amount
		  (first-denomination coin-values))
	       coin-values))))

(cc 100 us-coins)

(def us-coins (list 10 25 50 5 1))

(cc 100 us-coins)

;; ex 2.21

(defn same-parity [& args]
  (let [parity (first args)]
    (loop [arr args
	   result (list)]
      (cond
       (empty? arr) result
       (even? (+ parity (first arr))) (recur (rest arr) (conj result (first arr)))
       :else (recur (rest arr) result)))))
