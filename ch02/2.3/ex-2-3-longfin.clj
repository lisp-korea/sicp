(def length count)
(def a 1)
(def b 2)

(list a b)

(list 'a 'b)

(list 'a b)

(first '(a b c))

(rest '(a b c))

(def null? empty?)

(def eq? =)
(defn memq [item x]
  (loop [arr x]
    (cond (null? arr) false
	  (eq? item (first arr)) arr
	  :else (recur (rest arr)))))

(memq 'apple '(pear banana prune))

(memq 'apple '(x (apple sauce) y apple pear))

;; ex 2.53

(list 'a 'b 'c)
;; (a b c)

(list (list 'george))
;; ((george))

(rest '((x1 x2) (y1 y2)))
;; ((y1 y2))

(first (rest '((x1 x2) (y1 y2))))
;; (y1 y2)

(seq? (first '(a short list)))
;; false

(memq 'red '((red shoes) (blue socks)))
;; false

(memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

(defn equals? [a b]
  (if (and (and (not (null? a)) (seq? a))
	   (and (not (null? b)) (seq? b)))
    (and
     (eq? (first a) (first b))
     (equals? (rest a) (rest b)))
    (eq? a b)))

(equals? '(this is a list) '(this is a list))

(equals? '(this is a list) '(this (is a) list))

;; ex 2.55

(first ''abracadabra)
(first (quote (quote abracadabra)))



(defn variable? [e] (symbol? e))
(defn same-variable? [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))
(defn sum? [e]
  (and (seq? e)
       (= (first e) '+)))
(defn addend [e]
  (first (rest e)))
(defn augend [e]
  (first (rest (rest e))))
(defn make-sum [a1 a2]
  (list '+ a1 a2))
(defn product? [e]
  (and (seq? e)
       (= (first e) '*)))
(defn multiplier [e]
  (first (rest e)))
(defn multiplicand [e]
  (first (rest (rest e))))
(defn make-product [m1 m2]
  (list '* m1 m2))

(defn deriv [exp var]
  (cond (number? exp) 0
	(variable? exp) (if (same-variable? exp var) 1 0)
	(sum? exp) (make-sum (deriv (addend exp) var)
			     (deriv (augend exp) var))
	(product? exp) (make-sum
			(make-product (multiplier exp)
				      (deriv (multiplicand exp) var))
			(make-product (deriv (multiplier exp) var)
				      (multiplicand exp)))
	:else (print "unknown expression type -- DERIV" exp)))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
	(=number? a2 0) a1
	(and (number? a1) (number? a2)) (+ a1 a2)
	:else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
	(=number? m1 1) m2
	(=number? m2 1) m1
	(and (number? m1) (number? m2)) (* m1 m2)
	:else (list '* m1 m2)))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

;; ex 2.56

(defn exponention? [e]
  (and (seq? e)
       (= (first e) '**)))
(defn base [e]
  (first (rest e)))
(defn exponent [e]
  (first (rest (rest e))))
(defn make-exponention[base exponent]
  (cond (and (=number? exponent 0)) base
	(and (=number? base 1)) 1
	(and (number? base) (number? exponent)) (Math/pow base exponent)
	:else (list '** base exponent)))


(defn deriv [exp var]
  (cond (number? exp) 0
	(variable? exp) (if (same-variable? exp var) 1 0)
	(sum? exp) (make-sum (deriv (addend exp) var)
			     (deriv (augend exp) var))
	(product? exp) (make-sum
			(make-product (multiplier exp)
				      (deriv (multiplicand exp) var))
			(make-product (deriv (multiplier exp) var)
				      (multiplicand exp)))
	(exponention? exp) (make-product
			    (make-product
			     (exponent exp)
			     (make-exponention
			      (base exp)
			      (- (exponent exp) 1)))
			    (deriv (base exp) var))
	:else (print "unknown expression type -- DERIV" exp)))

(deriv '(** x 3) 'x)


;; ex 2.57

(defn addend [e]
  (first (rest e)))
(defn augend [e]
  (let [r (rest (rest e))]
    (if (= (length r) 1)
      (first r)
      (cons '+ r))))
(defn make-sum [& args]
  (loop [result '()
	 arr args]
    (let [f (first arr)]
      (cond (null? arr) (if (= (length result) 1)
			  (first result)
			  (cons '+ (reverse result)))
	    (= f 0) (recur result (rest arr))
	    :else (recur (cons f result) (rest arr))))))
(defn product? [e]
  (and (seq? e)
       (= (first e) '*)))
(defn multiplier [e]
  (first (rest e)))
(defn multiplicand [e]
  (let [r (rest (rest e))]
    (if (= (length r) 1)
      (first r)
      (cons '* (rest (rest e))))))
(defn make-product [& args]
  (loop [result '()
	 arr args]
    (let [f (first arr)]
      (cond (null? arr) (if (= (length result) 1) (first result) (cons '* (reverse result)))
	    (= f 0) 0
	    (= f 1) (recur result (rest arr))
	    :else (recur (cons f result) (rest arr))))))

;; xy(x + 3) => x^2y + 3x
(deriv '(* x y (+ x 3)) 'x)
;; (+ (* x y) (* y (+ x 3))) => xy + (y(x +3)) => xy + xy + 3y


;;ex 2.58

;;a

(defn addend [e]
  (first e))
(defn augend [e]
  (first (rest (rest e))))
(defn sum? [e]
  (and (seq? e)
       (= (first (rest e)) '+)))
(defn make-sum [a b]
  (cond (=number? a 0) b
	(=number? b 0) a
	(and (number? a) (number? b)) (+ a b)
	:else (list a '+ b)))
(defn product? [e]
  (and (seq? e)
       (= (first (rest e)) '*)))
(defn multiplier [e]
  (first e))
(defn multiplicand [e]
  (first (rest (rest e))))
(defn make-product [a b]
  (cond (or (=number? a 0) (=number? b 0)) 0
	(=number? a 1) b
	(=number? b 1) a
	(and (number? a) (number? b)) (* a b)
	:else (list a '* b)))

;; x+(3(x+(y+2)))
;; x+(3x+3y+6)
;; 4x+3y+6
(deriv '(x + (3 * (x + (y + 2)))) 'x)
;;4

;; b ???

(defn element-of-set? [x set]
  (cond (null? set) false
	(eq? x (first set)) true
	:else (element-of-set? x (rest set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn intersection-set [set1 set2]
  (cond (or (null? set1) (null? set2)) '()
	(element-of-set? (first set1) set2) (cons (first set1)
						  (intersection-set (rest set1) set2))
	:else (intersection-set (rest set1) set2)))

;; ex 2.59

(defn union-set [set1 set2]
  (cond (null? set1) set2
	(null? set2) set1
	(element-of-set? (first set1) set2) (union-set (rest set1) set2)
	:else (union-set (rest set1) (cons (first set1) set2))))

(union-set '(a b c) '(d e f))
;; (a b c d e f)
(union-set '() '(d e f))
;; (d e f)
(union-set '() '())
;; ()
(union-set '(a b c) '())
;; (a b c)
(union-set '(a b c) '(c d e))
;; (a b c d e)


;; ex 2.60

;; if we allow duplicates on set...

(def element-of-set? element-of-set?) ;; same

(defn adjoin-set [x set]
  (cons x set))
;; theta(1)

(defn union-set [set1 set2]
  (if (null? set1)
    set2
    (union-set
     (rest set1)
     (cons (first set1) set2))))
;; theta(n)


(defn without-set [e set]
  (loop [result '()
	 s set]
    (cond (null? s) result
	  (not (eq? e (first s))) (recur (cons (first s) result) (rest s))
	  :else (recur result (rest s)))))
;;theta(n)

(defn intersection-set [set1 set2]
  (cond (or (null? set1) (null? set2)) '()
	(element-of-set? (first set1) set2) (cons (intersection-set (rest set1) (without-set (first set1) set2)))
	:else (intersection-set (rest set1) set2)))
;;theta(n^3)?
  

(defn element-of-set? [x set]
  (cond (null? set) false
	(= x (first set)) true
	(< x (first set)) false
	:else (element-of-set? x (rest set))))

(defn intersection-set [set1 set2]
  (if (or (null? set1) (null? set2))
    '()
    (let [x1 (first set1)
	  x2 (first set2)]
      (cond (= x1 x2) (cons x1 (intersection-set (rest set1)
						 (rest set2)))
	    (< x1 x2) (intersection-set (rest set1) set2)
	    (< x2 x1) (intersection-set set1 (rest set2))))))


;; ex 2.61
(defn adjoin-set [x set]
  (cond
   (null? set) (cons x set)
   (< x (first set)) (cons x set)
   :else (cons (first set) (adjoin-set x (rest set)))))
;; theta(n)


(adjoin-set 5 []) ;;(5)
(adjoin-set 3 [1 2 3]) ;;(1 2 3 3)
(adjoin-set 3 [1 2 5]) ;;(1 2 3 5)


;; ex 2.62

(defn union-set [set1 set2]
  (loop [result '()
	 s1 set1
	 s2 set2]
    (let [x1 (first s1)
	  x2 (first s2)]
      (cond (and (null? s1) (null? s2)) (reverse result)
	    (null? s1) (recur (cons x2 result) s1 (rest s2)) ;; s2 isn't empty.
	    (null? s2) (recur (cons x1 result) (rest s1) s2) ;; s1 isn't empty.
	    (= x1 x2) (recur (cons x1 result) (rest s1) (rest s2))
	    (< x1 x2) (recur (cons x1 result) (rest s1) s2)
	    (> x1 x2) (recur (cons x2 result) s1 (rest s2))))))

(union-set '() '())
;;()

(union-set '(1 2 3) '())
;;(1 2 3)

(union-set '() '(1 2 3))
;;(1 2 3)

(union-set '(1 2 3) '(4 5 6))
;;(1 2 3 4 5 6)

(union-set '(1 2) '(2 3 4))
;;(1 2 3 4)


(defn entry [tree]
  (first tree))
(defn left-branch [tree]
  (first (rest tree)))
(defn right-branch [tree]
  (first (rest (rest tree))))
(defn make-tree [entry left right]
  (list entry left right))

(defn element-of-set? [x set]
  (cond (null? set) false
	(= x (entry set)) true
	(< x (entry set)) (element-of-set? x (left-branch set))
	(> x (entry set)) (element-of-set? x (right-branch set))))

(defn adjoin-set [x set]
  (cond (null? set) (make-tree x '() '())
	(= x (entry set)) set
	(< x (entry set)) (make-tree (entry set)
				     (adjoin-set x (left-branch set))
				     (right-branch set))
	(> x (entry set)) (make-tree (entry set)
				     (left-branch set)
				     (adjoin-set x (right-branch set)))))

;; ex 2.63
(def append concat)

(def tree-a
     (make-tree 7
		(make-tree 3
			   (make-tree 1 '() '())
			   (make-tree 5 '() '()))
		(make-tree 9
			   '()
			   (make-tree 11 '() '()))))

(def tree-b
     (make-tree 3
		(make-tree 1 '() '())
		(make-tree 7
			   (make-tree 5 '() '())
			   (make-tree 9
				      '()
				      (make-tree 11 '() '())))))

(def tree-c
     (make-tree 5
		(make-tree 3
			   (make-tree 1 '() '())
			   '())
		(make-tree 9
			   (make-tree 7 '() '())
			   (make-tree 11 '() '()))))
(defn tree->list-1 [tree]
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
	    (cons (entry tree)
		  (tree->list-1 (right-branch tree))))))

(defn tree->list-2 [tree]
  (defn copy-to-list [tree result-list]
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
		    (cons (entry tree)
			  (copy-to-list (right-branch tree)
					result-list)))))
  (copy-to-list tree '()))

(tree->list-1 tree-a)
;; (1 3 5 7 9 11)

(tree->list-1 tree-b)
;; (1 3 5 7 9 11)

(tree->list-1 tree-c)
;; (1 3 5 7 9 11)

(tree->list-2 tree-a)
;; (1 3 5 7 9 11)

(tree->list-2 tree-b)
;; (1 3 5 7 9 11)

(tree->list-2 tree-c)
;; (1 3 5 7 9 11)

;; a. all results are same.
;; b.
;; tree->list-1 : f(tree) = append(f(left), entry, f(right)) => theta(nlogn)
;; tree->list-2 : f(tree) = f'(left, entry + f'(right, result)) => theta(n)


;; ex 2.64
(defn quotient [x d]
  (int (/ x d)))
(defn partial-tree [elts n]
  (if (= n 0)
    (cons '() elts)
    (let [left-size (quotient (- n 1) 2)]
      (let [left-result (partial-tree elts left-size)]
	(let [left-tree (first left-result)
	      non-left-elts (rest left-result)
	      right-size (- n (+ left-size 1))]
	  (let [this-entry (first non-left-elts)
		right-result (partial-tree (rest non-left-elts)
					   right-size)]
	    (let [right-tree (first right-result)
		  remaining-elts (rest right-result)]
	      (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))
(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

(list->tree '(1 2 3 4 5))

;; a.
(list->tree '(1 2 3 4 5))
;; choose medain entry of list, and divide remaining element[left and right, remaining-elts for build right tree]. then apply left and right too. 

(cons (make-tree 3 (first (partial-tree '(1 2 3 4 5) 2)) (first (partial-tree '(4 5) 2))) '())
(cons (make-tree 3
		 (cons (make-tree 1 (first (partial-tree '(1 2) 0)) (first (partial-tree '(2) 1))) nil)
		 (cons (make-tree 4 (first (partial-tree '(4 5) 0)) (first (partial-tree '(5) 1))) nil)) nil)

;; b. T(n) = T(n/2) + 1 + T(n/2) => theta(n)


;; ex 2.65

(defn union-list [list1 list2]
  (loop [result '()
	 l1 list1
	 l2 list2]
    (cond (and (null? l1) (null? l2)) (reverse result)
	  (null? l1) (recur (cons (first l2) result) l1 (rest l2))
	  (null? l2) (recur (cons (first l1) result) (rest l1) l2)
	  (= (first l1) (first l2)) (recur (cons (first l1) result) (rest l1) (rest l2))
	  (> (first l1) (first l2)) (recur (cons (first l2) result) l1 (rest l2))
	  (< (first l1) (first l2)) (recur (cons (first l1) result) (rest l1) l2))))
(defn intersection-list [list1 list2]
  (loop [result '()
	 l1 list1
	 l2 list2]
    (cond (or (null? l1) (null? l2)) result
	  (= (first l1) (first l2)) (recur (cons (first l1) result) (rest l1) (rest l2))
	  (> (first l1) (first l2)) (recur result l1 (rest l2))
	  (< (first l1) (first l2)) (recur result (rest l1) l2))))
(defn union-set [set1 set2]
  (list->tree (union-list (tree->list-2 set1)
			  (tree->list-2 set2))))
(defn intersection-set [set1 set2]
  (list->tree (intersection-list (tree->list-2 set1)
				(tree->list-2 set2))))