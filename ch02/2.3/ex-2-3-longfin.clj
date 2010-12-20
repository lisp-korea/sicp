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
