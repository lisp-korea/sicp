(defn cube [x]
  (* x x x))

(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a (sum-integers (inc a) b))))

(defn sum-cubes [a b]
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (inc a) b))))

(defn pi-sum [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn sum-cubes [a b]
  (sum cube a inc b))

(defn sum-integer [a b]
  (sum identity a inc b))

(defn pi-sum [a b]
  (defn pi-term [x]
    (/ 1.0 (* x (+ 2 x))))
  (defn pi-next [x]
    (+ x 4))
  (sum pi-term a pi-next b))

(defn integral [f a b dx]
  (defn add-dx [x]
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


;; ex 1.29
(defn integral-simpson [f a b n]
  (defn h []
    (/ (- b a)
       n))
  (defn y [k]
    (f (+ a (* (h) k))))
  (defn yn [k]
    (cond
     (= k 0) (y k)
     (= k n) (y k)
     (even? k) (* 2 (y k))
     :else (* 4 (y k))))
  (* (/ (h) 3.0)
     (sum yn 0 inc n)))

;; ex 1.30

(defn sum [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))


(defn sum [term a next b]
  (loop [a a
	 result 0]
    (if (> a b)
      result
      (recur (next a) (+ result (term a))))))


;; ex 1.31
(defn product-recur [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product-recur term (next a) next b))))

(defn product-iter [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(defn product-iter [term a next b]
  (loop [a a
	 result 1]
    (if (> a b)
      result
      (recur (next a) (* (term a) result)))))

(defn square [n]
  (* n n))
(defn pi-product [n]
  (defn pi-next [n]
    (+ n 2))
  (defn term [x]
    (product-iter square x pi-next n))
  (if (even? n)
    (* 8
       (/ (term 4.0)
	  (* (inc n) (term 3.0))))
    (* 8
       (/ (* (inc n) (term 4.0))
	  (term 3.0)))))


;; ex 1.32
(defn accumulate-recur [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner a
	      (accumulate-recur combiner null-value term (next a) next b))))

(defn accumulate-iter [combiner null-value term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (combiner a result))))
  (iter a null-value))

(defn accumulate-loop [combiner null-value term a next b]
  (loop [a a
	 result null-value]
    (if (> a b)
      result
      (recur (next a) (combiner a result)))))
  (iter a null-value))
(defn sum-with-acc [term a next b]
  (accumulate-loop + 0 term a next b))
(defn product-with-acc [term a next b]
  (accumulate-recur * 1 term a next b))

;; ex 1.33

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

(defn filtered-accumualte [combiner null-value filter term a next b]
  (loop [a a
	 result null-value]
    (cond
     (> a b) result
     (filter a) (recur (next a) (combiner result a))
     :else (recur (next a) result))))

(defn sum-primes [a b]
  (filtered-accumualte + 0 prime? identity a inc b))
(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))
(defn product-coprimes [n]
  (defn coprime?[i]
    (= 1 (gcd i n)))
  (filtered-accumualte * 1 coprime? identity 1 inc n))


(defn pi-sum [a b]
  (sum #(/ 1.0 (* % (+ % 2)))
       a
       #(+ % 4)
       b))

(defn integral [f a b dx]
  (* (sum f
	  (+ a (/ dx 2.0))
	  #(+ % dx)
	  b)
     dx))

(defn f [x y]
  (defn f-helper [a b]
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
	    (- 1 y)))

(defn f [x y]
  (#(+ (* x (square %1))
       (* y %2)
       (* %1 %2))
   (+ 1 (* x y))
   (- 1 y)))

(defn f [x y]
  (let [a (+ 1 (* x y))
	b (- 1 y)]
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; ex 1.34
(defn f [g]
  (g 2))

;; (f square)
;; => (squre 2)
;; => 4

;; (f #(* % (+ % 1)))
;; => (#(* % (+ % 1)) 2)
;; => (* 2 (+ 2 1))
;; => 6

;; (f f)
;; => (f 2)
;; => (2 2)
;; => error(2 isn't function!)