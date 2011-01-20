(defn sqrt [x] (Math/sqrt x))
(defn square [x] (* x x))
(defn atan [x y] (Math/atan2 x y))
(defn cos [x] (Math/cos x))
(defn sin [x] (Math/sin x))

;; 2.5 Systems with Generic Operations

;; 2.5.1 Generic Arithmetic Operations
(defn attach-tag [type-tag contents]
  (cons type-tag contents))
(defn type-tag [datum]
  (if (seq? datum)
    (first datum)
    (println "Bad tagged datum -- TYPE-TAG" datum)))

(defn contents [datum]
  (if (seq? datum)
    (rest datum)
    (println "Bad tagged datum -- TYPET-AG" datum)))


(def table (atom {}))
(defn nget [op type]
  (get @table (list op type)))
(defn nput [op type item]
  (reset! table (assoc @table (list op type) item)))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)]
    (let [proc (nget op type-tags)]
      (if proc
	(apply proc (map contents args))
	(throw (Exception. (str "No method for these type --APPLY-GENERIC "
				op " " type-tags)))))))

(defn add [x y]
  (apply-generic 'add x y))
(defn sub [x y]
  (apply-generic 'sub x y))
(defn mul [x y]
  (apply-generic 'mul x y))
(defn div [x y]
  (apply-generic 'div x y))

(defn install-scheme-number-package []
  (letfn [(tag [x]
	       (attach-tag 'scheme-number x))]
    (nput 'add '(scheme-number scheme-number)
	  (fn [x y] (tag (+ x y))))
    (nput 'sub '(scheme-number scheme-number)
	  (fn [x y] (tag (- x y))))
    (nput 'mul '(scheme-number scheme-number)
	  (fn [x y] (tag (* x y))))
    (nput 'div '(scheme-number scheme-number)
	  (fn [x y] (tag (/ x y))))
    (nput 'make 'scheme-number
	  (fn [x] (tag x))))
  'done)

(defn make-scheme-number [n]
  ((nget 'make 'scheme-number) n))

(defn gcd [n d]
  (if (= d 0)
    n
    (gcd d (mod n d))))

(defn install-rational-package []
  (letfn [(numer [x]
		 (first x))
	  (denom [x]
		 (first (rest x)))
	  (make-rat [n d]
		    (let [g (gcd n d)]
		      (list (/ n g) (/ d g))))
	  (add-rat [x y]
		   (make-rat (+ (* (numer x) (denom y))
				(* (numer y) (denom x)))
			     (* (denom x) (denom y))))
	  (sub-rat [x y]
		   (make-rat (- (* (numer x) (denom y))
				(* (numer y) (denom x)))
			     (* (denom x) (denom y))))
	  (mul-rat [x y]
		   (make-rat (* (numer x) (numer y))
			     (* (denom x) (denom y))))
	  (div-rat [x y]
		   (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
	  
	  (tag [x] (attach-tag 'rational x))]
    (nput 'add '(rational rational)
	  (fn [x y] (tag (add-rat x y))))
    (nput 'sub '(rational rational)
	  (fn [x y] (tag (sub-rat x y))))
    (nput 'mul '(rational rational)
	  (fn [x y] (tag (mul-rat x y))))
    (nput 'div '(rational rational)
	  (fn [x y] (tag (div-rat x y))))
    (nput 'make 'rational
	  (fn [n d] (tag (make-rat n d)))))
  'done)

(defn make-rational [n d]
  ((nget 'make 'rational) n d))

(defn install-rectangular-package []
  ;; internal procedures
  (letfn [(real-part [z] (first z))
	  (imag-part [z] (first (rest z)))
	  (make-from-real-imag [x y] (list x y))
	  (magnitude [z]
		     (sqrt (+ (square (real-part z))
			      (square (imag-part z)))))
	  (angle [z]
		 (atan (imag-part z) (real-part z)))
	  (make-from-mag-ang [r a]
			     (list (* r (cos a)) (* r (sin a))))

	  (tag [x] (attach-tag 'rectangular x))]
    ;; interface to the rest of the system
    (nput 'real-part '(rectangular) real-part)
    (nput 'imag-part '(rectangular) imag-part)
    (nput 'magnitude '(rectangular) magnitude)
    (nput 'angle '(rectangular) angle)
    (nput 'make-from-real-imag 'rectangular
	  (fn [x y] (tag (make-from-real-imag x y))))
    (nput 'make-from-mag-ang 'rectangular
	  (fn [r a] (tag (make-from-mag-ang r a)))))
  'done)

;; Alyssa's package

(defn install-polar-package []
  ;;internal procedures
  (letfn [(magnitude [z] (first z))
	  (angle [z] (first (rest z)))
	  (make-from-mag-ang [r a] (list r a))
	  (real-part [z]
		     (* (magnitude z) (cos (angle z))))
	  (imag-part [z]
		     (* (magnitude z) (sin (angle z))))
	  (make-from-real-imag [x y]
			       (list (sqrt (+ (square x) (square y)))
				     (atan y x)))

	  (tag [x] (attach-tag 'polar x))]
    ;;interface to the rest of the system    
    (nput 'real-part '(polar) real-part)
    (nput 'imag-part '(polar) imag-part)
    (nput 'magnitude '(polar) magnitude)
    (nput 'angle '(polar) angle)
    (nput 'make-from-real-imag 'polar
	  (fn [x y] (tag (make-from-real-imag x y))))
    (nput 'make-from-mag-ang 'polar
	  (fn [r a] (tag (make-from-mag-ang r a)))))    
  'done)

(defn real-part [z] (apply-generic 'real-part z))
(defn imag-part [z] (apply-generic 'imag-part z))
(defn magnitude [z] (apply-generic 'magnitude z))
(defn angle [z] (apply-generic 'angle z))

(defn make-from-real-imag [x y]
  ((nget 'make-from-real-imag 'rectangular) x y))

(defn make-from-mag-ang [r a]
  ((nget 'make-from-mag-ang 'polar) r a))


(defn install-complex-package []
  (letfn [(make-from-real-imag [x y]
			       ((nget 'make-from-real-imag 'rectangular) x y))
	  (make-from-mag-ang [r a]		      
			     ((nget 'make-from-mag-ang 'polar) r a))
	  (add-complex [z1 z2]
		       (make-from-real-imag (+ (real-part z1) (real-part z2))
					    (+ (imag-part z1) (imag-part z2))))
	  (sub-complex [z1 z2]
		       (make-from-real-imag (+ (real-part z1) (real-part z2))
					    (+ (imag-part z1) (imag-part z2))))
	  (mul-complex [z1 z2]
		       (make-from-mag-ang (* (magnitude z1) (magnitude z2))
					  (+ (angle z1) (angle z2))))
	  (div-complex [z1 z2]
		       (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))

	  (tag [z] (attach-tag 'complex z))]
    (nput 'add '(complex complex)
	  (fn [z1 z2] (tag (add-complex z1 z2))))
    (nput 'sub '(complex complex)
	  (fn [z1 z2] (tag (sub-complex z1 z2))))
    (nput 'mul '(complex complex)
	  (fn [z1 z2] (tag (mul-complex z1 z2))))
    (nput 'div '(complex complex)
	  (fn [z1 z2] (tag (div-complex z1 z2))))
    (nput 'make-from-real-imag 'complex
	  (fn [x y] (tag (make-from-real-imag x y))))
    (nput 'make-from-mag-ang 'complex
	  (fn [r a] (tag (make-from-mag-ang r a)))))
  'done)

(defn make-complex-from-real-imag [x y]
  ((nget 'make-from-real-imag 'complex) x y))
(defn make-complex-from-mag-ang [r a]
  ((nget 'make-from-mag-ang 'complex) r a))

;; ex 2.77
(def z (make-complex-from-real-imag 3 4))

(nput 'angle '(complex) angle)
(nput 'magnitude '(complex) magnitude)
(nput 'real-part '(complex) real-part)
(nput 'imag-part '(complex) imag-part)

 (magnitude z)
(apply-generic 'magnitude z)
(let [type-tags '(complex)]
  (let [proc (nget 'magnitude type-tags)]
    (if proc
      (apply proc (map contents (list z)))
      (throw (Exception. (str "No method for these type --APPLY-GENERIC "
			      'magnitude " " type-tags)))))))
(apply magnitude (map contents (list z)))

(magnitude (first (map contents (list z))))
(apply-generic 'magnitude (first (map contents (list z))))

(let [proc (nget 'magnitude '(rectangular))]
  (if proc
    (apply proc (map contents (map contents (list z))))
    (throw (Exception. (str "No method for these type --APPLY-GENERIC ")))))


;; from rectangular package
(letfn [(real-part [z] (first z))
	  (imag-part [z] (first (rest z)))
	  (make-from-real-imag [x y] (list x y))
	  (magnitude [z]
		     (sqrt (+ (square (real-part z))
			      (square (imag-part z)))))]
	  (apply (fn [z]
		   (sqrt (+ (square (real-part z))
			    (square (imag-part z))))) (map contents (map contents (list z)))))


;; apply-generic was invoked twice.


;; ex 2.78

(defn attach-tag [type-tag contents]
  (if (number? contents)
    contents
    (cons type-tag contents)))
(defn type-tag [datum]
  (cond
   (number? datum) 'scheme-number
   (seq? datum) (first datum)
   :else (println "Bad tagged datum -- TYPE-TAG" datum)))

(defn contents [datum]
  (cond
   (number? datum) datum
   (seq? datum) (rest datum)
   :else (println "Bad tagged datum -- TYPET-AG" datum)))

(install-scheme-number-package)
((nget 'add '(scheme-number scheme-number)) 3 (make-scheme-number 10))

;; ex 2.79 & 2.80
(defn equ? [a b]
  (apply-generic 'equ? a b))

(defn =zero? [a]
  (apply-generic '=zero? a))

;; add to scheme-number


(defn install-scheme-number-package []
  (letfn [(tag [x]
	       (attach-tag 'scheme-number x))
	  (equ? [a b]
		(= a b))
	  (=zero? [a] (= a 0))]
    (nput 'add '(scheme-number scheme-number)
	  (fn [x y] (tag (+ x y))))
    (nput 'sub '(scheme-number scheme-number)
	  (fn [x y] (tag (- x y))))
    (nput 'mul '(scheme-number scheme-number)
	  (fn [x y] (tag (* x y))))
    (nput 'div '(scheme-number scheme-number)
	  (fn [x y] (tag (/ x y))))
    (nput 'make 'scheme-number
	  (fn [x] (tag x)))
    (nput 'equ? '(scheme-number scheme-number) equ?)
    (nput '=zero? '(scheme-number) =zero?))
  'done)

(install-scheme-number-package)
(equ? 3 5) ;;false
(equ? 3 3) ;;true
(equ? 3 (make-scheme-number 4)) ;; false
(equ? 3 (make-scheme-number 3)) ;; true

(=zero? 0)
(=zero? (make-scheme-number 0))
;; true

(=zero? 1)
(=zero? (make-scheme-number 3))
;; false

;; add to rational
(defn install-rational-package []
  (letfn [(numer [x]
		 (first x))
	  (denom [x]
		 (first (rest x)))
	  (make-rat [n d]
		    (let [g (gcd n d)]
		      (list (/ n g) (/ d g))))
	  (add-rat [x y]
		   (make-rat (+ (* (numer x) (denom y))
				(* (numer y) (denom x)))
			     (* (denom x) (denom y))))
	  (sub-rat [x y]
		   (make-rat (- (* (numer x) (denom y))
				(* (numer y) (denom x)))
			     (* (denom x) (denom y))))
	  (mul-rat [x y]
		   (make-rat (* (numer x) (numer y))
			     (* (denom x) (denom y))))
	  (div-rat [x y]
		   (make-rat (* (numer x) (denom y))
			     (* (denom x) (numer y))))
	  (equ? [x y]
		(and (= (numer x) (numer y))
		     (= (denom x) (denom y))))
	  (=zero? [x]
		  (= (numer x) 0))
	  (tag [x] (attach-tag 'rational x))]
    (nput 'add '(rational rational)
	  (fn [x y] (tag (add-rat x y))))
    (nput 'sub '(rational rational)
	  (fn [x y] (tag (sub-rat x y))))
    (nput 'mul '(rational rational)
	  (fn [x y] (tag (mul-rat x y))))
    (nput 'div '(rational rational)
	  (fn [x y] (tag (div-rat x y))))
    (nput 'equ? '(rational rational) equ?)
    (nput '=zero? '(rational) =zero?)
    (nput 'make 'rational
	  (fn [n d] (tag (make-rat n d)))))
  'done)

(install-rational-package)

(equ? (make-rational 4 1) (make-rational 16 3)) ;;false
(equ? (make-rational 4 1) (make-rational 16 4)) ;;true

(=zero? (make-rational 1 10)) ;; false
(=zero? (make-rational 0 15)) ;; true

;; add to complex
(defn install-complex-package []
  (letfn [(make-from-real-imag [x y]
			       ((nget 'make-from-real-imag 'rectangular) x y))
	  (make-from-mag-ang [r a]		      
			     ((nget 'make-from-mag-ang 'polar) r a))
	  (add-complex [z1 z2]
		       (make-from-real-imag (+ (real-part z1) (real-part z2))
					    (+ (imag-part z1) (imag-part z2))))
	  (sub-complex [z1 z2]
		       (make-from-real-imag (+ (real-part z1) (real-part z2))
					    (+ (imag-part z1) (imag-part z2))))
	  (mul-complex [z1 z2]
		       (make-from-mag-ang (* (magnitude z1) (magnitude z2))
					  (+ (angle z1) (angle z2))))
	  (div-complex [z1 z2]
		       (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
					  (- (angle z1) (angle z2))))
	  (enough-close? [x y]
			 (< (Math/abs (- x y)) 0.001))
	  (equ? [z1 z2]
		(and (enough-close? (real-part z1) (real-part z2))
		     (enough-close? (imag-part z1) (imag-part z2))))
	  (=zero? [z]
		  (and (enough-close? (real-part z) 0)
		       (enough-close? (imag-part z) 0)))
	  (tag [z] (attach-tag 'complex z))]
    (nput 'add '(complex complex)
	  (fn [z1 z2] (tag (add-complex z1 z2))))
    (nput 'sub '(complex complex)
	  (fn [z1 z2] (tag (sub-complex z1 z2))))
    (nput 'mul '(complex complex)
	  (fn [z1 z2] (tag (mul-complex z1 z2))))
    (nput 'div '(complex complex)
	  (fn [z1 z2] (tag (div-complex z1 z2))))
    (nput 'make-from-real-imag 'complex
	  (fn [x y] (tag (make-from-real-imag x y))))
    (nput 'equ? '(complex complex) equ?)
    (nput '=zero? '(complex) =zero?)
    (nput 'make-from-mag-ang 'complex
	  (fn [r a] (tag (make-from-mag-ang r a)))))
  'done) 

(install-complex-package)

(equ? (make-complex-from-real-imag 5 10)
      (make-complex-from-real-imag 5 11))
;;false

(equ? (make-complex-from-real-imag 5 10)
      (make-complex-from-real-imag 5 10))
;;true

(equ? (make-complex-from-mag-ang 5 0.9272)
      (make-complex-from-real-imag 3 4))
;;true

(=zero? (make-complex-from-real-imag 1 10)) ;; false
(=zero? (make-complex-from-real-imag 0 0)) ;; true

(=zero? (make-complex-from-mag-ang 0 0)) ;; true
(=zero? (make-complex-from-mag-ang 0 10)) ;; true

