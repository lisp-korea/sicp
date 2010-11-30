
(defn make-rat [n d]
  (list n d))
(defn numer [x]
  (first x))
(defn denom [x]
  (first (rest x)))

(defn print-rat [x]
  (print (numer x) "/" (denom x)))
(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(def one-half (make-rat 1 2))

(print-rat one-half)

(def one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(defn gcd [x y]
  (loop [x x
	 y y]
    (if (= y 0)
      (Math/abs x)
      (recur y (rem x y)))))

(defn make-rat [n d]
  (let [g (gcd n d)]
    (list (/ n g) (/ d g))))

;; ex 2.1

(defn make-rat [n d]
  (let [g ((if (< d 0) - +) (gcd n d))] 
    (list (/ n g) (/ d g))))

 