;;; Let Over Lambda에 나오는 매크로입니다.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 사용 예
(let ((lst (list '((((1) 2) (3 4))) '((5 6) (7 8)) 9 10 11)))
  (with-all-cxrs
      (cons
       (cadadadr lst)
       (caaaaar lst))))

(macroexpand-1 '(with-all-cxrs
		 (cons
		  (cadadadr lst)
		  (caaaaar lst))))
;; (LABELS ((CADADADR (L)
;;            (CXR (1 A 1 D 1 A 1 D 1 A 1 D) L))
;;          (CAAAAAR (L)
;;            (CXR (1 A 1 A 1 A 1 A 1 A) L)))
;;   (CONS (CADADADR LST) (CAAAAAR LST)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-all-cxrs (&rest forms)
  `(labels
       (,@(mapcar
	   (lambda (s)
	     `(,s (l)
		  (cxr ,(cxr-symbol-to-cxr-list s)
		       l)))
	   (remove-duplicates
	    (remove-if-not
	     #'cxr-symbol-p
	     (flatten forms)))))
     ,@forms))

(cxr-symbol-to-cxr-list 'caddadr) ;; (1 a 1 d 1 d 1 a 1 d)

(defun cxr-symbol-to-cxr-list (s)
  (labels ((collect (l)
	     (if l
		 (list*
		  1
		  (if (char= (car l) #\A)
		      'A
		      'D)
		  (collect (cdr l))))))
    (collect
	(cdr      ; chop off C
	 (butlast ; chop off R
	  (coerce
	   (symbol-name s)
	   'list))))))

(defun cxr-symbol-p (s)
  (if (symbolp s)
      (let ((chars (coerce 
		    (symbol-name s)
		    'list)))
	(and
	 (< 6 (length chars))
	 (char= #\C (car chars))
	 (char= #\R (car (last chars)))
	 (null (remove-if
		(lambda (c)
		  (or (char= c #\A)
		      (char= c #\D)))
		(cdr (butlast chars))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 사용 예
(cxr (1 a 4 d) '(1 2 3 4 5))
(cxr (5 a 11 d) '(1 2 3 4 5 6 7 8 9 10 11 (((((12 13)))))))

(def-english-list-accessors 11 20)

(eleventh '(1 2 3 4 5 6 7 8 9 10 11)) ; 11

(macroexpand-1 '(def-english-list-accessors 11 20))
;; (PROGN
;;   (DEFUN ELEVENTH (ARG) (CXR (1 A 10 D) ARG))
;;   (DEFUN TWELFTH (ARG) (CXR (1 A 11 D) ARG))
;;   (DEFUN THIRTEENTH (ARG) (CXR (1 A 12 D) ARG))
;;   (DEFUN FOURTEENTH (ARG) (CXR (1 A 13 D) ARG))
;;   (DEFUN FIFTEENTH (ARG) (CXR (1 A 14 D) ARG))
;;   (DEFUN SIXTEENTH (ARG) (CXR (1 A 15 D) ARG))
;;   (DEFUN SEVENTEENTH (ARG) (CXR (1 A 16 D) ARG))
;;   (DEFUN EIGHTEENTH (ARG) (CXR (1 A 17 D) ARG))
;;   (DEFUN NINETEENTH (ARG) (CXR (1 A 18 D) ARG))
;;   (DEFUN TWENTIETH (ARG) (CXR (1 A 19 D) ARG)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-english-list-accessors (start end)
  (if (not (<= 1 start end))
      (error "Bad start/end range"))
  `(progn
     ,@(loop for i from start to end collect
	    `(defun
		 ,(symb
		   (map 'string
			(lambda (c)
			  (if (alpha-char-p c)
			      (char-upcase c)
			      #\-))
			(format nil "~:r" i)))
		 (arg)
	       (cxr (1 a ,(- i 1) d) arg)))))

;;(defvar cxr-inline-thresh 10)
(defvar cxr-inline-thresh 10)

;; 숫자가 10이상이면 루프형태로
(macroexpand-1 '(cxr (10 a) l))
;; (LET ()
;;   (NLET-TAIL #:NAME1136 ((#:COUNT1137 10) (#:VAL1138 (CXR NIL L)))
;;              (IF (>= 0 #:COUNT1137) #:VAL1138
;;                  (#:NAME1136 (- #:COUNT1137 1) (CAR #:VAL1138)))))

(defmacro! cxr (x tree)
  (if (null x)
      tree
      (let ((op (cond
		  ((eq 'a (cadr x)) 'car)
		  ((eq 'd (cadr x)) 'cdr)
		  (t (error "Non A/D symbol")))))
	(if (and (integerp (car x))
		 (<= 1 (car x) cxr-inline-thresh))
	    (if (= 1 (car x))
		`(,op (cxr ,(cddr x) ,tree))
		`(,op (cxr ,(cons (- (car x) 1) (cdr x))
			   ,tree)))
	    `(nlet-tail
	      ,g!name ((,g!count ,(car x))
		       (,g!val (cxr ,(cddr x) ,tree)))
	      (if (>= 0 ,g!count)
		  ,g!val
		  ;; Will be a tail
		  (,g!name (- ,g!count 1)
			   (,op ,g!val))))))))



;;; 위의 매크로는 cxr을 구현하기 위한 매크로입니다.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 아래의 매크로는 범용 매크로입니다.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nlet tail recursive version

;;; test - nlet-tail-fact 
(defun nlet-tail-fact (n)
  (nlet-tail fact ((n n) (acc 1))
	     (if (zerop n)
		 acc
		 (fact (- n 1) (* acc n)))))

(nlet-tail-fact 4)

(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (loop for i in letargs
		 collect (gensym))))
    `(macrolet
	 ((,n ,gs
	    `(progn
	       (psetq
		,@(apply #'nconc
			 (mapcar
			  #'list
			  ',(mapcar #'car letargs)
			  (list ,@gs))))
	       (go ,',g!n))))
       (block ,g!b
	 (let ,letargs
	   (tagbody
	      ,g!n (return-from
		    ,g!b (progn ,@body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; once only
;; defmacro!

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
	 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
	  ,(progn ,@body)))))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
		"O!"
		:start1 0
		:end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
	(subseq (sybmol-name s) 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defmacro/g!

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
	       (remove-if-not #'g!-symbol-p
			      (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
	      (lambda (s)
		`(,s (gensym ,(subseq
			       (symbol-name s)
			       2))))
	      syms)
	 ,@body))))

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
		"G!"
		:start1 0
		:end1 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utility

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec
		       (car x)
		       (rec (cdr x) acc))))))
    (rec x nil)))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
