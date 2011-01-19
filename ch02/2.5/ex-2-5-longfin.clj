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
(nput 'magnitude '(complex) magnitude)
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
