
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

(defn make-rat [n d]
  (list n d))

(defn numer [x]
  (let [g (gcd (first x) (last rest x))]
    (/ (first x) g)))

(defn denom [x]
  (let [g (gcd (first x) (last x))]
    (/ (first (rest x)) g)))

;; ex 2.2
(defn make-segment [start-segment end-segment]
  (list start-segment end-segment))

(defn make-point [x y]
  (list x y))
(defn x-point [p]
  (first p))

(defn y-point [p]
  (last p))

(defn print-point [p]
  (newline)
  (print "(" (x-point p) "," (y-point p) ")"))

(defn midpoint-segment [segment]
  (make-segment (/ (+ (x-point (first segment))
		      (x-point (last segment))) 2)
		(/ (+ (y-point (first segment))
		      (y-point (last segment))) 2)))
(defn length [segment]
  (let [a (first segment)
	b (last segment)]
    (Math/sqrt
     (+
      (Math/pow
       (- (x-point b)
	  (x-point a)) 2)
      (Math/pow
       (- (y-point b)
	  (y-point a)) 2)))))
;; ex 2.3

(defn make-rentangle [w h]
  (list w h))

(defn perimeter [r]
  (let [w (first r)
	h (last r)]
    (* (+ (length w)
	  (length h)) 2)))

(defn area [r]
  (let [w (first r)
	h (last r)]
    (* h w)))


(defn new-cons [x y]
  (defn dispatch [m]
    (cond (= m 0) x
	  (= m 1) y
	  :else (print "Argument not 0 or 1 -- CONS" m)))
  dispatch)

(defn car [z]
  (z 0))

(defn cdr [z]
  (z 1))

;; ex 2.4

(defn ex-cons [x y]
  (fn [m] (m x y)))

(defn ex-car [z]
  (z (fn [p q] p)))

(defn ex-cdr [z]
  (z (fn [p q] q)))

(ex-car (ex-cons 4 5))
(ex-cdr (ex-cons 4 5))

;; ex 2.5

(defn int-cons [a b]
  (* (Math/pow 2 a)
     (Math/pow 3 b)))

(defn max-divisor [x d]
  (loop [i 1]
    (if (= 0 (rem x (Math/pow d i)))
      (recur (inc i))
      (- i 1))))
(defn int-car [x]
  (max-divisor x 2))
(defn int-cdr [x]
  (max-divisor x 3))

;; ex 2.6

(def zero (fn [f] (fn [x] x)))

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

;; (add-1 zero)
;; (fn [f] (fn [x] (f ((zero f) x))))
;; (fn [f] (fn [x] (f (((fn [f] (fn [x] x)) f) x))))
;; (fn [f] (fn [x] (f (fn [x] x) x)))
;; (fn [f] (fn [x] (f x)))

(def one (fn [f] (fn [x] (f x))))

;; (add-1 one)
;; (fn [f] (fn [x] (f ((one f) x))))
;; (fn [f] (fn [x] (f (((fn [a] (fn [b] (a b))) f) x))))
;; (fn [f] (fn [x] (f ((fn [b] (f b)) x))))
;; (fn [f] (fn [x] (f (f x))))

(def two (fn [f] (fn [x] (f (f x)))))

(def three (fn [f] (fn [x] (f (f (f x))))))

(defn add [a b]
  (fn [f]
    (fn [x]
      ((a f) ((b f) x)))))

(= ((one inc) 0) 1)
(= ((two inc) 0) 2)
(= ((three inc) 0) 3)
(= (((add one two) inc) 0) 3)


;; ex 2.7

(defn make-interval [a b]
  (list a b))

(defn upper-bound [i]
  (max (first i) (last i)))

(defn lower-bound [i]
  (min (first i) (last i)))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
	p2 (* (lower-bound x) (upper-bound y))
	p3 (* (upper-bound x) (upper-bound y))
	p4 (* (upper-bound x) (lower-bound y))]

    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
(defn div-interval [x y]
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(add-interval (make-interval 4 10)
	      (make-interval -5 -10))
(mul-interval (make-interval 5 10)
	      (make-interval -4 20))

;; ex 2.8

(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(sub-interval (make-interval 4 10)
	      (make-interval -5 -1))

;; ex 2.9

(defn width [x]
  (/ (- (upper-bound x)
	(lower-bound x)) 2.0))

(def x (make-interval -5 5))
(def y (make-interval -10 10))

(= (width (add-interval x y))
   (+ (width x) (width y)))

(= (width (mul-interval x y))
   (* (width x) (width y)))

(def y (make-interval 0 15))

(= (width (mul-interval x y))
   (* (width x) (width y)))

;; ex 2.10

(defn div-interval [x y]
  (if (>= 0 (* (upper-bound y)
	       (lower-bound y)))
    (print "error: divide by interval contains 0")
    (mul-interval x
		  (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y))))))

(div-interval x y)

;; ex 2.11

;; x  y  r
;; ========
;; ++ ++ ++ ll' uu'
;; ++ +- +- ul' lu'
;; ++ -- -- uu' ll'
;; +- ++ -+ lu' ul'
;; +- +- -+ min(ul', lu') max(uu', ll')
;; +- -- -+ ul' lu' 
;; -- ++ -- lu' ul'
;; -- +- -+ lu' ul'
;; -- -- ++ uu' ll'

(defn mul-interval [x y]
  (let [lx (lower-bound x)
	ux (upper-bound x)
	ly (lower-bound y)
	uy (upper-bound y)]
    (cond
     ;; ++/++
     (and (pos? lx) (pos? ux) (pos? ly) (pos? uy))
     (make-interval (* lx ly) (* ux uy))
     ;; ++/+-
     (and (pos? lx) (pos? ux) (neg? ly) (pos? uy))
     (make-interval (* ux ly) (* lx uy))
     ;; ++/--
     (and (pos? lx) (pos? ux) (neg? ly) (neg? uy))
     (make-interval (* ux uy) (* lx ly))
     ;; +-/++
     (and (neg? lx) (pos? ux) (pos? ly) (pos? uy))
     (make-interval (* lx uy) (* ux ly))
     ;; +-/+-
     (and (neg? lx) (pos? ux) (neg? ly) (pos? uy))
     (make-interval (min (* lx uy) (* ux ly))
		    (max (* lx ly) (* ux uy)))
     ;; +-/--
     (and (neg? lx) (pos? ux) (neg? ly) (pos? uy))
     (make-interval (* ux ly) (* lx uy))
     ;; --/++
     (and (neg? lx) (neg? ux) (pos? ly) (pos? uy))
     (make-interval (* lx uy) (ux ly))
     ;; --/+-
     (and (neg? lx) (neg? ux) (neg? ly) (pos? uy))
     (make-interval (* lx uy) (* ux ly))
     ;; --/--
     (and (neg? lx) (neg? ux) (neg? ly) (neg? uy))
     (make-interval (* ux uy) (* lx ly)))))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;; ex 2.12
(defn make-center-percent [c p]
  (let [offset (* c (/ p 100))
	lower (- c offset)
	upper (+ c offset)]
    (make-interval lower upper)))

(defn percent [i]
  (let [c (center i)
	offset (- (upper-bound i) c)]
    (/ (* 100 offset) c)))
    
;; ex 2.13
;; c, o => (c-o, c+o)
;; c', o' => (c'-o', c'+o')
;; p1 = (c-o)(c'-o') = cc' + oo'
;; p2 = (c-o)(c'+o') = cc' - oo'
;; p3 = (c+o)(c'-o') = cc' - oo'
;; p4 = (c+o)(c'+o') = cc' + oo'

;; if o and o' are positive, 
;; min = cc' - oo'
;; max = cc' + oo'
;; complicated offset is oo'

;; ex 2.14

(def a (make-center-percent 10 0.005))
(def b (make-center-percent 100 0.005))

(div-interval a a)
(div-interval a b)

;; ex 2.15
;; ex 2.16
;; ex 2.17
