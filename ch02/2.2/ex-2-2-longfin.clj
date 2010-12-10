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


(defn scale-list [items factor]
  (if (empty? items)
    nil
    (cons (* (first items) factor)
	  (scale-list (rest items) factor))))

(defn mapn [proc items]
  (if (empty? items)
    nil
    (cons (proc (first items))
	  (mapn proc (rest items)))))

(defn scale-list [items factor]
  (mapn (fn [x] (* x factor))
	items))

;; ex 2.21

(defn square-list [items]
  (if (empty? items)
    nil
    (cons (* (first items) (first items))
	  (square-list (rest items)))))

(defn square-list [items]
  (mapn (fn [x] (* x x)) items))

;; ex 2.22

(defn square-list [items]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (iter (rest things)
	    (cons (* (first things) (first things))
		  answer))))
  (iter items nil))

;; (square-list '(1 2 3))
;; (iter '(1 2 3) nil)
;; (iter '(2 3) '(1))
;; (iter '(3) '(4 1))
;; (iter '() '(9 4 1))
;; '(9 4 1)

(defn square-list [items]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (iter (rest things)
	    (cons answer
		  (* (first things) (first things))))))
  (iter items nil))

;; (cons x xs) not (cons xs x)

;; ex 2.23
(defn for-each [proc items]
  (if (not (empty? items))
    (do (proc (first items))
	(for-each proc (rest items)))))


(def x (cons (list 1 2) (list  3 4)))
(length x)

(defn null? [x]
  (cond (nil? x) true
	(and (seq? x) (empty? x)) true
	:else false))(
(defn count-leaves [x]
  (cond (null? x) 0
	(not (seq? x)) 1
	:else (+ (count-leaves (first x))
		 (count-leaves (rest x)))))
(count-leaves x)

(count-leaves (list x x))

;; ex 2.24

(list 1 (list 2 (list 3 4)))

;; [1 [2 [3 4]]]

;; ex 2.25

(def a '(1 3 (5 7)))
(def b '((7)))
(def c '(1 (2 (3 (4 (5 (6 7)))))))

(first (rest (first (rest (rest a)))))
(first (first b))
(first (rest (first (rest (first (rest (first (rest (first (rest (first (rest c))))))))))))

(def x (list 1 2 3))
(def y (list 4 5 6))

(append x y)
;; (1 2 3 4 5 6)

(cons x y)
;; ((1 2 3) 4 5 6)

(list x y)
;; ((1 2 3) (4 5 6))

;; ex 2.27
(defn deep-reverse [items]
  (loop [arr items
	 result (list)
	 ]
    (let [target (first arr)
	  reversed (if (seq? target) (deep-reverse target) target)]
      (if (= (length items) (length result))
	result
	(recur (rest arr) (append (list reversed) result))))))


;; ex 2.28

(defn fringe [x]
  (loop [arr x
	 result (list)]
    (let [target (first arr)
	  flatten (if (seq? target) (fringe target) (list target))]
      (print target)
      (newline)
      (if (null? arr)
	result
	(recur (rest arr) (append result flatten))))))
	  
;; ex 2.29

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

;; a

(defn left-branch [m]
  (first m))
(defn right-branch [m]
  (last m))

(defn branch-length [b]
  (first b))
(defn branch-structure [b]
  (last b))


(defn mobile? [x]
  (seq? x))
;; b

(defn total-weight [m]
  (let [left (branch-structure (left-branch m))
	left-weight (if (seq? left) (total-weight left) left)
	right (branch-structure (right-branch m))
	right-weight (if (seq? right) (total-weight right) right)]
    (+ left-weight right-weight)))

;; c
(defn balanced? [m]
  (let [left (branch-structure (left-branch m))
	right (branch-structure (right-branch m))]
    (cond (and (seq? left) (not (balanced? left))) false
	  (and (seq? right) (not (balanced? right))) false
	  :else
	  (let [left-weight (if (seq? left) (total-weight left) left)
		right-weight (if (seq? right) (total-weight right) right)]
	    (= (* left-weight (branch-length (left-branch m)))
	       (* right-weight (branch-length (right-branch m))))))))
    
;; d
;; only change left-branch, right-branch, branch-lenght and branch-structure


(defn scale-tree [tree factor]
  (cond (null? tree) nil
	(not (seq? tree)) (* tree factor)
	:else (cons (scale-tree (first tree) factor)
		    (scale-tree (rest tree) factor))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(defn scale-tree [tree factor]
  (mapn (fn [sub-tree]
	  (if (seq? sub-tree)
	    (scale-tree sub-tree factor)
	    (* sub-tree factor)))
	tree))


(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

;; ex 2.30
(defn sqaure-tree [tree]
  (cond (null? tree) nil
	(not (seq? tree)) (* tree tree)
	:else (cons (sqaure-tree (first tree))
		    (sqaure-tree (rest tree)))))

(sqaure-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(defn square-tree [tree]
  (mapn (fn [sub-tree]
	  (if (seq? sub-tree)
	    (square-tree sub-tree)
	    (* sub-tree sub-tree)))
	tree))

;; ex 2.31

(defn tree-map [proc tree]
  (mapn (fn [sub-tree]
	  (if (seq? sub-tree)
	    (tree-map proc sub-tree)
	    (proc sub-tree)))
	tree))

(defn square-tree [tree]
  (tree-map (fn [x] (* x x)) tree))

;; ex 2.32

(defn subsets [s]
  (if (null? s)
    (list '())
    (let [r (subsets (rest s))]
      (append r (mapn (fn [x] (cons (first s) x)) r)))))

(subsets (list 3))
;; (subsets (rest (list 3)))
;; (subsets ())
;; ()
;; (() (3))

(subsets (list 2 3))
;; (subsets (rest (list 2 3)))
;; (subsets (rest (list 3)))
;; (() (3)) <= apply 2
;; (() (3) (2) (2 3))

(subsets (list 1 2 3))
;; (subsets (rest (list 1 2 3)))
;; (subsets (rest (list 2 3)))
;; (() (3) (2) (2 3)) <= apply 1
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))