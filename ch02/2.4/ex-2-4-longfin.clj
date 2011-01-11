;; 2.4.1 Representations for Complex Numbers

;; (make-from-real-imag (real-part z) (imag-part z))
;; (make-from-mag-ang (magnitude z) (angle z))

(defn sqrt [x] (Math/sqrt x))
(defn square [x] (* x x))
(defn atan [x y] (Math/atan2 x y))
(defn cos [x] (Math/cos x))
(defn sin [x] (Math/sin x))

;; Ben's representation
;; '(real image)

(defn real-part [z] (first z))
(defn imag-part [z] (first (rest z)))
(defn magnitude [z]
  (sqrt (+ (square (real-part z))
	   (square (imag-part z)))))
(defn angle [z]
  (atan (imag-part z) (real-part z)))

(defn make-from-real-imag [r i] (list r i))
(defn make-from-mag-ang [r a]
  (list (* r (cos a)) (* r (sin a))))

;; Alyssa's representation
;; '(magnitude angle)

(defn magnitude [z] (first z))
(defn angle [z] (first (rest z)))
(defn real-part [z]
  (* (magnitude z) (cos (angle z))))
(defn imag-part [z]
  (* (magnitude z) (sin (angle z))))
(defn make-from-real-imag [x y]
  (list (sqrt (+ (square x) (square y)))
	(atan y x)))
(defn make-from-mag-ang [r a] (list r a))

(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(defn sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

;; 2.4.2 Tagged data
(defn attach-tag [type-tag contents]
  (cons type-tag contents))

(defn type-tag [datum]
  (if (seq? datum)
    (first datum)
    (println "Bad tagged datum -- TYPE-TAG" datum)))

(defn contents [datum]
  (if (seq? datum)
    (rest datum)
    (println "Bad tagged datum -- TYPE-TAG" datum)))
    

(defn rectangular? [z]
  (= (type-tag z) 'rectangular))
(defn polar? [z]
  (= (type-tag z) 'polar))

;; Ben's representation[rectangular]

(defn real-part-rectangular [z]
  (first z))
(defn imag-part-rectangular [z]
  (first (rest z)))
(defn magnitude-rectangular [z]
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))
(defn angle-rectangular [z]
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))
(defn make-from-real-imag-rectangular [x y]
  (attach-tag 'rectangular (list x y)))
(defn make-from-mag-ang-rectangular [r a]
  (attach-tag 'rectangular
	      (list (* r (cos a)) (* r (sin a)))))

;; Alyssa's representation[polar]

(defn magnitude-polar [z]
  (first z))
(defn angle-polar [z]
  (first (rest z)))
(defn real-part-polar [z]
  (* (magnitude-polar [z]) (cos (angle-polar z))))
(defn imag-part-polar [z]
  (* (magnitude-polar [z]) (sin (angle-polar z))))
(defn make-from-real-imag-polar [x y]
  (attach-tag 'polar
	      (list (sqrt (+ (square x) (square y)))
		    (atan y x))))
(defn make-from-mag-ang-polar [r a]
  (attach-tag 'polar
	      (list r a)))


;; generic selectors

(defn real-part [z]
  (cond (rectangular? z) (real-part-rectangular (contents z))
	(polar? z) (real-part-polar (contents z))
	:else (throw (Exception. (str "Unknown type -- REAL-PART" z)))))
(defn imag-part [z]
  (cond (rectangular? z) (imag-part-rectangular (contents z))
	(polar? z) (imag-part-polar (contents z))
	:else (throw (Exception. (str "Unknown type -- IMAG-PART" z)))))
(defn magnitude [z]
  (cond (rectangular? z) (magnitude-rectangular (contents z))
	(polar? z) (magnitude-polar (contents z))
	:else (throw (Exception. (str "Unknown type -- MAGNITUDE" z)))))
(defn angle [z]
  (cond (rectangular? z) (angle-rectangular (contents z))
	(polar? z) (angle-polar (contents z))
	:else (throw (Exception. (str "Unknown type -- ANGLE" z)))))

(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(defn make-from-real-imag [x y]
  (make-from-real-imag-rectangular x y))

(defn make-from-mag-ang [r a]
  (make-from-mag-ang-polar r a))


;; 2.4.3 Data-Directed Programming and Additivity

(def table (atom {}))
(defn nget [op type]
  (get @table (list op type)))
(defn nput [op type item]
  (reset! table (assoc @table (list op type) item)))


;; Ben's package

(defn install-rectangular-package []
  ;; internal procedures
  (defn real-part [z] (first z))
  (defn imag-part [z] (first (rest z)))
  (defn make-from-real-imag [x y] (list x y))
  (defn magnitude [z]
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (defn angle [z]
    (atan (imag-part z) (real-part z)))
  (defn make-from-mag-ang [r a]
    (list (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (defn tag [x] (attach-tag 'rectangular x))

  (nput 'real-part '(rectangular) real-part)
  (nput 'imag-part '(rectangular) imag-part)
  (nput 'magnitude '(rectangular) magnitude)
  (nput 'angle '(rectangular) angle)
  (nput 'make-from-real-imag 'rectangular
       (fn [x y] (tag (make-from-real-imag x y))))
  (nput 'make-from-mag-ang 'rectangular
       (fn [r a] (tag (make-from-mag-ang r a))))
  'done)

;; Alyssa's package

(defn install-polar-package []
  ;;internal procedures
  (defn magnitude [z] (first z))
  (defn angle [z] (first (rest z)))
  (defn make-from-mag-ang [r a] (list r a))
  (defn real-part [z]
    (* (magnitude z) (cos (angle z))))
  (defn imag-part [z]
    (* (magnitude z) (sin (angle z))))
  (defn make-from-real-imag [x y]
    (list (sqrt (+ (square x) (square y)))
	  (atan y x)))

  ;;interface to the rest of the system
  (defn tag [x] (attach-tag 'polar x))

  (nput 'real-part '(polar) real-part)
  (nput 'imag-part '(polar) imag-part)
  (nput 'magnitude '(polar) magnitude)
  (nput 'angle '(polar) angle)
  (nput 'make-from-real-imag 'polar
       (fn [x y] (tag (make-from-real-imag x y))))
  (nput 'make-from-mag-ang 'polar
       (fn [r a] (tag (make-from-mag-ang r a))))

  'done)
(defn apply-genric [op & args]
  (let [type-tags (map type-tag args)]
    (let [proc (nget op type-tags)]
      (if proc
	(apply proc (map contents args))
	(throw (Exception. "No method for these type --APPLY-GENERIC"
			   (str op type-tags)))))))

(defn real-part [z] (apply-genric 'real-part z))
(defn imag-part [z] (apply-genric 'imag-part z))
(defn magnitude [z] (apply-genric 'magnitude z))
(defn angle [z] (apply-genric 'angle z))

(defn make-from-real-imag [x y]
  ((nget 'make-from-real-imag 'rectangular) x y))

(defn make-from-mag-ang [r a]
  ((nget 'make-from-mag-ang 'polar) r a))


;; ex 2.73

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
(defn product? [e]
  (and (seq? e)
       (= (first e) '*)))
(defn multiplier [e]
  (first (rest e)))
(defn multiplicand [e]
  (first (rest (rest e))))

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


(defn operator [exp] (first exp))

(defn operands [exp] (rest exp))

(defn deriv [exp var]
  (cond (number? exp) 0
	(variable? exp) (if (same-variable? exp var) 1 0)
	:else ((nget 'deriv (operator exp)) (operands exp) var)))


;; a. number? and variable? are general rule. can't be specified by operator.

;; b

(defn deriv-product [exp var]
  (defn multiplier [exp] (first exp))
  (defn multiplicand [exp] (first (rest exp)))
  (make-sum
   (make-product (multiplier exp)
		 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
		 (multiplicand exp))))

(defn deriv-sum [exp var]
  (defn addend [exp] (first exp))
  (defn augend [exp] (first (rest exp)))
  (make-sum (deriv (addend exp) var)
	    (deriv (augend exp) var)))

;; put deriv-sum and deriv-product
(nput 'deriv '+ deriv-sum)
(nput 'deriv '* deriv-product)

(deriv (make-sum (make-product 'x 3) 5) 'x)

;; c

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

(defn deriv-exponent [exp var]
  (defn base [exp] (first exp))
  (defn exponent [exp] (first (rest exp)))
  (make-product
   (make-product
    (exponent exp)
    (make-exponention
     (base exp)
     (- (exponent exp) 1)))
   (deriv (base exp) var)))

;; put deriv-exponent
(nput 'deriv '** deriv-exponent)

;; d nothing.