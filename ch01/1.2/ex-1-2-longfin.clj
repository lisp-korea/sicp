;;ex 1.9

;; (recur-add 3 5)
;; (inc (recur-add 2 5))
;; (inc (inc (recur-add 1 5)))
;; (inc (inc (inc (recur-add 0 5))))
;; (inc (inc (inc 5)))
;; (inc (inc 6))
;; (inc 7)
;; 8
(defn recur-add [a b]
  (if (= a 0)
    b
    (inc (recur-add (dec a) b))))

;; (tail-recur-add 3 5)
;; (tail-recur-add 2 6)
;; (tail-recur-add 1 7)
;; (tail-recur-add 0 8)
;; 8
(defn tail-recur-add [a b]
  (if (= a 0)
    b
    (tail-recur-add (dec a) (inc b))))

;;ex 1.10

(defn A [x y]
  (cond (= y 0) 0
	(= x 0) (* 2 y)
	(= y 1) 2
	:else (A (- x 1)
		 (A x (- y 1)))))

;; user> (A 1 10)
;; (A 0 (A 1 9))
;; (* 2 (A 1 9))
;; (* 2 (A 0 (A 1 8)))
;; (* 2 (* 2 (A 0 (A 1 7))))
;; (* 2 (* 2 (* 2 (A 0 (A 1 6)))))
;; ...
;; 1024
;; user> (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (* 2 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (* 2 (* 2 (* 2 2))))
;; (A 1 16)
;; (A 0 (A 1 15))
;; ...
;; (A 0 (A 0 (A 1 14)))
;; ...
;; 65536
;; user> (A 3 3)
;; 65536

(defn f [n]
  (A 0 n))

;; (f 3)
;; (A 0 3)
;; (* 2 3)
;; (f n) => 2n

(defn g [n]
  (A 1 n))

;; (g 3)
;; (A 1 3)
;; (A 0 (A 1 2)) == (A 0 (g 2))
;; (* 2 (A 0 (A 1 1))) == (* 2 (A 0 (g 1)))
;; (* 2 (* 2 2))
;; 8
;; (g n) => 2^n
 
(defn h [n]
  (A 2 n))
;; (h 3)
;; (A 2 3)
;; (A 1 (A 2 2))
;; (g (A 2 2)) == (g (h 2))
;; (g (A 1 (A 2 1)))
;; (g (g (A 2 1))) == (g (g (h 1)))
;; (g (g 2))
;; (g 4)
;; 16
;; (h n) => ?

