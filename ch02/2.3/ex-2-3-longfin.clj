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
