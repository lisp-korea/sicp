;; 3.2 The Environment Model of Evaluation

;; 3.2.1 The Rules for Evaluation

(defn square [x]
  (* x x))

;; equals

(def square
     (fn [x] (* x x)))
"
                     +----------------------+
                     | other variables      |
          global --->|                      |
          env        | square: --+          |
                     +-----------|----------+
                                 |       ^
          (define (square x)     |       |
            (* x x))             V       |
                             .---.---.   |
                             | O | O-+---+
                             `-|-^---'
                               |
                               V
                             parameters: x
                             body: (* x x)
"


;; 3.2.2 Applying Simple Procedures

(defn sqaure [x]
  (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(defn f [a]
  (sum-of-squares (+ a 1) (* a 2)))

"
                    +--------------------------------------------+
                    | sum-of-squares:                            |
          global -->| square:                                    |
          env       | f: --+                                     |
                    +------|--------------+--------------+-------+
                           |     ^        |     ^        |     ^
                           |     |        |     |        |     |
                           V     |        V     |        V     |
                       .---.---. |    .---.---. |    .---.---. |
                       | O | O-+-+    | O | O-+-+    | O | O-+-+
                       `-|-^---'      `-|-^---'      `-|-^---'
                         |              |              |
                         V              V              V
             parameters: a          parameters: x  parameters: x, y
             body: (sum-of-squares  body: (* x x)  body: (+ (square x)
                     (+ a 1)                                (square y))
                     (* a 2))

			evaluating (f 5)

                    +-----------------------------------------------------+
          global -->|                                                     |
          env       +-----------------------------------------------------+
                      ^              ^                ^               ^
          (f 5)       |              |                |               |
                  +------+       +-------+        +------+        +-------+
            E1 -->| a: 5 |  E2 ->| x: 6  |  E3 -->| x: 6 |  E4 -->| x: 10 |
                  |      |       | y: 10 |        |      |        |       |
                  +------+       +-------+        +------+        +-------+
             (sum-of-squares   (+ (square x)       (* x x)         (* x x)
               (+ a 1)            (square u))
               (+ a 2))
"

;; ex 3.9

(defn factorial [n]
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(factorial 6)

;; E1 [n 6] (if (=n 1) 1(* n (factorial (- n 1))))
;; E2 [n 5] (if (=n 1) 1(* n (factorial (- n 1))))
;; E3 [n 4] (if (=n 1) 1(* n (factorial (- n 1))))
;; E4 [n 3] (if (=n 1) 1(* n (factorial (- n 1))))
;; E5 [n 2] (if (=n 1) 1(* n (factorial (- n 1))))
;; E6 [n 1] (if (=n 1) 1(* n (factorial (- n 1))))

;; E1 ~ E6 point global environment.

(defn factorial-iter [product counter max-count]
  (if (> counter max-count)
    product
    (factorial-iter
     (* counter product)
     (+ counter 1)
     max-count)))
(defn factorial [n]
  (factorial-iter 1 1 n))

(factorial 6)

;; E1 [n 6] (factorial-iter 1 1 n)
;; E2 [counter 1, product 1, max-count 6] (if ...)
;; E3 [counter 2, product 1, max-count 6] (if ...)
;; E4 [counter 3, product 2, max-count 6] (if ...)
;; E5 [counter 4, product 6, max-count 6] (if ...)
;; E6 [counter 5, product 24, max-count 6] (if ...)
;; E7 [counter 6, product 120, max-count 6] (if ...)
;; E8 [counter 7, product 720, max-count 6] (if ...)

;; E1 ~ E8 point global environment.
