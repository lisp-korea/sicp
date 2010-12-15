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
    (cons (first items) (reverse-recur (rest items)))))
(reverse-recur (list 10 4 9))

(defn reverse-iter [items]
  (loop [arr items
	 reversed (list)
	 ]
    (if (= (length items) (length reversed))
      reversed
      (recur (rest arr) (cons (first arr) reversed)))))

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
	:else false))
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
	(recur (rest arr) (cons reversed result))))))


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


;; 2.2.3

(defn sum-odd-squares [tree]
  (cond (null? tree) 0
	(not (seq? tree)) (if (odd? tree) (* tree tree) 0)
	:else (+ (sum-odd-squares (first tree))
		 (sum-odd-squares (rest tree)))))

(sum-odd-squares (list 1 (list 2 3 (list 4 5))))


(defn fib [n]
  (loop [a 0
	 b 1
	 c 1]
    (cond (= n 0) 0
	  (< n 2) 1
	  (= n c) b
	  :else (recur b (+ a b) (+ c 1)))))
(defn even-fibs [n]
  (defn _next[k]
    (if (> k n)
      nil
      (let [f (fib k)]
	(if (even? f)
	  (cons f (_next (+ k 1)))
	  (_next (+ k 1))))))
  (_next 0))

(defn filtern [pred seq]
  (cond (null? seq) nil
	(pred (first seq)) (cons (first seq)
				 (filtern pred (rest seq)))
	:else (filtern pred (rest seq))))

(filtern odd? (list 1 2 3 4 5))

(defn filtern-iter [pred seq]
  (loop [result (list)
	 arr seq]
    (cond (null? arr) (reverse-iter result)
	  (pred (first arr)) (recur (cons (first arr) result) (rest arr))
	  :else (recur result (rest arr)))))

(filtern-iter odd? (list 1 2 3 4 5))

(defn accumulate [op initial seq]
  (if (null? seq)
    initial
    (op (first seq)
	(accumulate op initial (rest seq)))))

(accumulate + 0 (list 1 2 3 4 5))

(accumulate * 1 (list 1 2 3 4 5))

(accumulate cons nil (list 1 2 3 4 5))

(defn accumulate-iter [op initial seq]
  (loop [result initial
	 arr seq]
    (if (null? arr)
      result
      (recur (op (first arr) result) (rest arr)))))

(accumulate-iter + 0 (list 1 2 3 4 5))

(accumulate-iter * 1 (list 1 2 3 4 5))

(accumulate-iter cons nil (list 1 2 3 4 5)) ;; reversed


(defn enumerate-interval [low high]
  (if (> low high)
    nil
    (cons low (enumerate-interval (inc low) high))))

(enumerate-interval 2 7)

(defn enumerate-interval-iter [low high]
  (loop [n low
	 result nil]
    (if (> n high)
      (reverse-iter result)
      (recur (inc n) (cons n result)))))

(enumerate-interval-iter 2 7)

(defn enumerate-tree [tree]
  (cond (null? tree) nil
	(not (seq? tree)) (list tree)
	:else (append (enumerate-tree (first tree))
		      (enumerate-tree (rest tree)))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(defn sum-odd-squares [tree]
  (accumulate-iter +
		   0
		   (mapn (fn [x] (* x x))
			 (filtern-iter odd?
				       (enumerate-tree tree)))))

(sum-odd-squares (list 1 (list 2 3 (list 4 5))))

(defn even-fibs [n]
  (accumulate cons
	      nil
	      (filtern-iter even?
			    (mapn fib
				  (enumerate-interval-iter 0 n)))))

(even-fibs 10)


(defn list-fib-squares [n]
  (accumulate cons
	      nil
	      (mapn (fn [x] (* x x))
		    (mapn fib
			  (enumerate-interval-iter 0 n)))))

(list-fib-squares 10)

(defn product-of-squares-of-odd-elements [seq]
  (accumulate *
	      1
	      (mapn (fn [x] (* x x))
		    (filtern-iter odd? seq))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))

;; (defn salary-of-highest-paid-programmer [records]
;;   (accumulate max
;; 	      0
;; 	      (map salary
;; 		   (filter programmer? records))))

;; ex 2.33

(defn mapn [p seq]
  (accumulate (fn [x y] (cons (p x) y)) nil seq))

(mapn inc (list 1 2 3))

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))

(defn length [seq]
  (accumulate (fn [x y] (inc y)) 0 seq))

(length (list 1 2 3 4 5 6))


;; ex 2.34

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms]
		(+ this-coeff
		   (* x
		      (horner-eval x (rest coefficient-sequence)))))
	      0
	      coefficient-sequence))

;; (x=1) 1 + 1*1 => 1
(horner-eval 1 (list 1 1))

;; (x=2) 2 + x*2 => 6
(horner-eval 2 (list 2 2))

;; (x=3) 1+ 3x + 5x^3 + x^5 => 388
(horner-eval 3 (list 1 3 0 5 0 1))


;; ex 2.35

(defn count-leaves [t]
  (accumulate (fn [x y] (+ x y))
	      0
	      (mapn (fn [e]
		      (cond (null? e) 0
			    (not (seq? e)) 1
			    :else (count-leaves e))) t)))

(count-leaves (list 1))
(count-leaves (list 1 2 3))
(count-leaves (list 1 (list 2 3) 4))
(count-leaves (list 1 (list 2 3 (list 4 5) 6) 7))

;; ex 2.36

(defn accumulate-n [op init seqs]
  (if (null? (first seqs))
    nil
    (cons (accumulate op init (mapn first seqs))
	  (accumulate-n op init (mapn rest seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))


;; ex 2.37

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map (fn [r]
	 (dot-product r v)) m))

(matrix-*-vector (list (list 1 0 2)
		       (list -1 3 1)) (list 3 2 1))

(defn transpose [m]
  (accumulate-n cons nil m))

(transpose (list (list 3 1)
		 (list 2 1)
		 (list 1 0)))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [w]
	   (map (fn [v]
		  (dot-product w v)) cols)) m)))

(matrix-*-matrix (list (list 1 0 2)
		       (list -1 3 1))
		 (list (list 3 1)
		       (list 2 1)
		       (list 1 0)))

;; ex 2.38

(defn fold-left [op initial sequence]
  (loop [result initial
	 arr sequence]
    (if (null? arr)
      result
      (recur (op result (first arr))
	     (rest arr)))))
(defn fold-right [op initial sequence]
  (loop [result initial
	 arr sequence]
    (if (null? arr)
      result
      (recur (op (first arr) result)
	     (rest arr)))))

(fold-right / 1 (list 1 2 3))
;; 3/2

(fold-left / 1 (list 1 2 3))
;; 1/6


(fold-right list nil (list 1 2 3))

(fold-left list nil (list 1 2 3))

;; op's assertion (= (op a b) (op b a))


;; ex 2.39

(defn reverse-l [sequence]
  (fold-left (fn [x y] (cons y x)) nil sequence))

(reverse-l (list 1 2 3))

(defn reverse-r [sequence]
  (fold-right (fn [x y] (cons x y)) nil sequence))

(reverse-r (list 1 2 3))

(def n 10)
(accumulate append
	    nil
	    (map (fn [i]
		   (map (fn [j] (list i j))
			(enumerate-interval 1 (- i 1))))
		 (enumerate-interval 1 n)))

(defn flatmap [proc seq]
  (accumulate append nil (mapn proc seq)))

(flatmap (fn [i] (map (fn [j] (list i j))
		      (enumerate-interval 1 (- i 1))))
	 (enumerate-interval 1 10))

(defn square [x]
  (* x x))
(defn divides? [a b]
  (= (mod b a) 0))
(defn find-divisor [n test-divisor]
  (loop [n n
	 test-divisor test-divisor]
    (cond (> (square test-divisor) n) n
	  (divides? test-divisor n) test-divisor
	  :else (recur n (+ test-divisor 1)))))
(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))


(defn prime-sum? [pair]
  (prime? (+ (first pair) (first (rest pair)))))

(prime-sum? (list 3 5))
(prime-sum? (list 2 3))

(defn make-pair-sum [pair]
  (list (first pair)
	(first (rest pair))
	(+ (first pair)
	   (first (rest pair)))))

(make-pair-sum (list 3 5))

(defn prime-sum-pairs [n]
  (mapn make-pair-sum
	(filtern-iter prime-sum?
		      (flatmap
		       (fn [i]
			 (mapn (fn [j] (list i j))
			       (enumerate-interval 1 (- i 1))))
		       (enumerate-interval 1 n)))))
(prime-sum-pairs 6)

(defn removen [item seq]
  (filter (fn [x] (not (= x item)))
	  seq))
(defn permutations [s]
  (if (null? s)
    (list nil)
    (flatmap (fn [x]
	       (map (fn [p] (cons x p))
		    (permutations (removen x s))))
	     s)))

(permutations (list 1 2 3))
;; (map (fn [x] (map (fn [p] (cons x p)) (permutations (removen x s)))) '(1 2 3))
;;

;; ex 2.40

(defn unique-pair [n]
  (flatmap (fn [i]
	     (mapn (fn [j] (list i j))
		   (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(unique-pair 5)

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum? (unique-pair n))))

(prime-sum-pairs 6)

;; ex 2.41

(defn unique-triples [n]
  (flatmap (fn [i]
	      (map (fn [j]
		     (first (map (fn [k]
			    (list i j k))
			  (enumerate-interval 1 (- j 1)))))
		   (enumerate-interval 1 (- i 1))))
	    (enumerate-interval 1 n)))

(unique-triples 5)
(defn get-consists [n s]
  (filtern
   (fn [e]
     (if (null? e)
       false
       (=
	(+ (first e) (first (rest e)) (first (rest (rest e))))
	s)))
   (unique-triples n)))

(get-consists 10 6)
(get-consists 10 7)
(get-consists 10 8)