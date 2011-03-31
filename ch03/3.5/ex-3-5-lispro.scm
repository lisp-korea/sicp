##########Exercise 3.50

(defun stream-map (proc &rest argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (mapcar #'stream-car argstreams))
      (apply #'stream-map
        (cons proc (mapcar #'stream-cdr argstreams))))))

(deflex s1 (stream-enumerate-interval 10 100))
(deflex s2 (stream-enumerate-interval 20 200))
(deflex s3 (stream-enumerate-interval 30 300))

(deflex ss (stream-map #'+ s1 s2 s3))

(stream-ref ss 0)
#=> 60
(stream-ref ss 1)
#=> 63
(stream-ref ss 2)
#=> 66

#############Exercise 3.51

(deflex x (stream-map #'show (stream-enumerate-interval 0 10))))
#=> 0
(stream-ref x 5)
#=>
#1
#2
#3
#4
#5
(stream-ref x 7)
#=>
#6
#7

########Exercise 3.52

(deflex sum 0)
(defun accum (x)
  (setf sum (+ x sum))
  sum)
#=> sum is 0

(deflex seq (stream-map #'accum (stream-enumerate-interval 1 20)))
#=> sum is 1

(deflex y (stream-filter #'evenp seq))
#=> sum is 6

(deflex z (stream-filter (lambda (x) (= (rem x 5) 0)) seq))
#=> sum is 10

(stream-ref y 7)
#=> 136
#=> sum is 136

(display-stream z)
#=>
#10
#15
#45
#55
#105
#120
#190
#210

###########Exercise 3.53

(deflex s (cons-stream 1 (add-streams s s)))
#=> 1, 2, 4, 8, 16 ...


############Exercise 3.54

(defun mul-streams (s1 s2)
  (stream-map #'* s1 s2))

(deflex factorials
  (cons-stream 1 (mul-streams
                    factorials
                    (integers-starting-from 2))))



#############Exercise 3.55


(defun partial-sums (s)
  (cons-stream
    (stream-car s)
    (add-streams
      (stream-cdr s)
      (partial-sums s))))
Exercise 3.56

(defun merge (s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (t
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                    (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                    (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (t
                    (cons-stream
                      s1car
                      (merge (stream-cdr s1) (stream-cdr s2)))))))))

(deflex s
  (cons-stream
    1
    (merge
      (scale-stream integers 2)
      (merge
        (scale-stream integers 3)
        (scale-stream integers 5)))))


##########Exercise 3.57

With memoization, n-1 additions are performed for computing the n@th fibonacci number, since each call of @force on the stream returned by add-streams recomputes the fibs stream only once.

Without memoization, the growth is exponential because in the call to add-streams, (stream-cdr fibs) will do all the work fibs does, but that is repeated.

##########Exercise 3.58

(expand 1 7 10)
#=> 1 4 2 8 5 7 1 4 2 8

(expand 3 8 10)

#=> 3 7 5 0 0 0 0 0 0 0


########Exercise 3.59

#a.

(defun integrate-series (s)
  (labels (
      (integrate-aux (s n)
        (cons-stream
          (/ (stream-car s) n)
          (integrate-aux
            (stream-cdr s)
            (+ n 1)))))
    (integrate-aux s 1)))


# b.

(deflex sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(deflex cosine-series
  (cons-stream 1
    (scale-stream
      (integrate-series sine-series)
      -1)))


######Exercise 3.60


(defun mul-series (s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams
      (scale-stream (stream-cdr s2) (stream-car s1))
      (mul-series (stream-cdr s1) s2))))
###########################

(deflex la
  (add-streams
    (mul-series sine-series sine-series)
    (mul-series cosine-series cosine-series)))
#=> 1 0 0 0 ...


##########Exercise 3.61

(defun invert-unit-series (sr)
  (cons-stream
    1
    (scale-stream
      (mul-series
        (invert-unit-series sr)
        (stream-cdr sr))
      -1)))

########Exercise 3.62

(defun div-series (num denom)
  (let ((denom-const (stream-car denom)))
    (if (zerop denom-const)
      (error "denom constant term is zero")
      (mul-series
        (invert-unit-series
          (scale-stream denom denom-const))
        num))))

(deflex tangent-series
  (div-series sine-series cosine-series))