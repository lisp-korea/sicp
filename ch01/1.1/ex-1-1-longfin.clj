;; sicp exercise(ch 1.1)

;; ex 1.1
10 ;10
(+ 5 3 4) ;12
(- 9 1) ;8
(+ (* 2 4) (- 4 6)) ;6
(def a 3)
(def b (+ a 1))
(+ a b (* a b)) ;19
(= a b) ;false
(if
    (and (> b a) (< b (* a b)))
  b
  a) ;4
(cond
 (= a 4) 6
 (= b 4) (+ 6 7 a)
 :else 25) ;16
(+ 2 (if (> b a) b a)) ;6
(* (cond
    (> a b) a
    (< a b) b
    :else -1)
   (+ a 1)) ;16

;;ex1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;;ex1.3

(defn sum-of-two-large-number [& args]
  (let
      [sorted (sort > args)
       first (nth sorted 0)
       second (nth sorted 1)]
    (letfn
	[(square [v] (* v v))]
      (+ (square first)
	 (square second)))))



