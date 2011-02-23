;ex 3.9-1
;           --------------------
;          | factorial :        |
;           --------------------
;            |   ^
;             -  |
;              |  -
;              v   |
;             ㅇㅇ-
;             |
;             v
;        Parameters : n
;        Body :..............
;
;Global     ---------------------------------------------------------------------------------
;env    -> |                                                                                 |
;           ---------------------------------------------------------------------------------
;	     ^	            ^	           ^	          ^              ^              ^
;	     |	            |	           |	          |              |              |
;	  -------	 -------	-------	       -------        -------        -------
;   E1 ->| n : 6 | E2 ->| n : 5 | E3 ->| n : 4 | E4 ->| n : 3 | E5 ->| n : 2 | E6 ->| n : 1 |
;	  -------	 -------	-------	       -------        -------        -------
;
;	  (if (= n 1)
;	  1
;	  (* n (factorial (- n 1))))

;ex 3.9-2
;              --------------
;             |              |
;           --|--------------|------
;          | fact-iter :     |      |
;          | factorial :     |      |
;           -------^---------|----^- 
;            |     |         |    |
;             -    |         |    |
;              |   |         |    |
;              v   |         v    |
;             ㅇㅇ-        ㅇㅇ---
;             |             |
;             v              ---->   Pameters : product
;        Parameters : n              Counter max-count
;        Body :(fact-iter 1 1 n)     Body :..........
;
;Global     ------------------------------------------------------------------------------------------------
;env    -> |										                    |
;           ------------------------------------------------------------------------------------------------
;	     ^	             ^	              ^	               ^                ^                ^
;	     |	             |	              |	               |                |                |
;	  -------	 ---------	  ---------        ---------        ---------        ---------
;   E1 ->| n : 6 | E2 ->|product:1| E3 ->|product:1| E4 ->|product:1| E5 ->|product:1| E6 ->|product:1|
;	  -------       |counter:1|      |counter:2|      |counter:3|      |counter:4|      |counter:5|.....
;(fact-iter 1 1 n)      |max-     |      |max-     |      |max-     |      |max-     |      |max-     |
;                       |count : 6|      |count : 6|      |count : 6|      |count : 6|      |count : 6|
;			 ---------	  ---------	   ---------        ---------        ---------
;                  (if (> counter max-count)
;                   Product
;		    (fact-iter (* counter product)
;		    (+ counter 1) max-count))

;ex 3.10
;
;
;           --------------------------------------------------------
;          | make-withdraw --------------------------               |
;          | W1----                                  |              |
;           -------|--------------^- ----------------|------^-------
;                  |              |                  |      |
;                  |          ------------------     v      |
;                --       E1 |initial-amount:100|   ㅇㅇ----
;               |     E2      --------^---------    |
;               |      |              |             |
;               |    --               |             |
;               |   |    -----------  |             |
;               |    -->|balance:100|-              V
;               |        ----^------         Parameters : initial-amount
;               |            |               body :
;             ㅇㅇ-----------                 ((lambda (balance)
;	      |				       (lambda (amount)
;	       -			        (if (>=balance amount)
;		|				  (begin (set! balace (- balace amount))
;		v				      balance)
;	parameters : amount			  "Insufficient funds"))) initial-amount)
;       body :
;       (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;         balance)
;       "Insufficient funds")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(W1 50)
;           --------------------------------------------------------
;          | make-withdraw --------------------------               |
;          | W1----                                  |              |
;           -------|--------------^- ----------------|------^-------
;                  |              |                  |      |
;                  |          ------------------     v      |
;                --       E1 |initial-amount:100|   ㅇㅇ----
;               |     E2      --------^---------    |
;               |      |              |             |
;               |    --               |             |
;               |   |    -----------  |             |
;               |    -->|balance:100|-              V
;               |        ----^---^--					Parameters : initial-amount
;               |            |   |					body :
;             ㅇㅇ-----------    |					 ((lambda (balance)
;	      |			 ---------				  (lambda (amount)
;	       -		|amount:50|				   (if (>=balance amount)
;		|		 ---------				    (begin (set! balace (- balace amount))
;		v		if( (>=balance amount)			      balance)
;				 (begin (set! balance (- balace amount))  "Insufficient funds"))) initial-amount)
;	parameters : amount        balance)
;       body :			 "Insufficient funds")
;       (if (>= balance amount)  
;        (begin (set! balance (- balance amount))
;         balance)
;       "Insufficient funds")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           --------------------------------------------------------
;          | make-withdraw --------------------------               |
;          | W1----                                  |              |
;           -------|--------------^- ----------------|------^-------
;                  |              |                  |      |
;                  |          ------------------     v      |
;                --       E1 |initial-amount:100|   ㅇㅇ----
;               |     E2      --------^---------    |
;               |      |              |             |
;               |    --               |             |
;               |   |     ----------  |             |
;               |    --> |balance:50|-              V
;               |         ---^------         Parameters : initial-amount
;               |            |               body :
;             ㅇㅇ-----------                 ((lambda (balance)
;	      |				       (lambda (amount)
;	       -			        (if (>=balance amount)
;		|				  (begin (set! balace (- balace amount))
;		v				      balance)
;	parameters : amount			  "Insufficient funds"))) initial-amount)
;       body :
;       (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;         balance)
;       "Insufficient funds")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           ----------------------------------------------------------------------------------------------
;          | make-withdraw ----------------------------------------------------------------------         |
;          | W1----                                                                              |        |
;          | W2----|---------------------------------                                            |        |
;           -------|--------------^- ----------------|------^-----------------------^------------|----^---
;                  |              |                  |E4    |                       |            |    |
;                  |          ------------------     ||   -----------       ------------------   |    |
;                --       E1 |initial-amount:100|    | ->|balance:100| E3->|initial-amount:100|  |    |
;               |     E2      --------^---------     |    -----------       --------^---------   v    |
;               |      |              |              |      |    |                  |           ㅇㅇ--
;               |    --               |             ㅇㅇ----      ------------------             |
;               |   |    ----------   |              |             Parameters : initial-amount<--
;               |    -->|balance:50|--               V             body :
;               |        ---^------  Parameters : amount                     (lambda (balance)
;               |           |        body :                                   (lambda (amount)
;             ㅇㅇ----------          (if (>= balance amount)                  (if (>=balance amount)
;	      |			       (begin (set! balace (- balace amount))   (begin (set! balace (- balace amount))
;	       -			balance)                                 balance)
;		|			"Insufficient funds")	             "Insufficient funds"))) initial-amount)
;		v				      
;	parameters : amount			  
;       body :
;       (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;         balance)
;       "Insufficient funds")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (cond ((number? amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
            (else (display initial-amount)
                  (display "  ")
                  (display balance)
                  (newline)))))) 

(define W1 (make-withdraw 100))
(W1 'd)
(W1 50)
(W1 'd)
(newline)
(define W2 (make-withdraw 100))
(W2 'd)

;ex3-11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           --------------------------------------------------------
;          | make-account ----------------------------------        |
;          | acc-- -                                        |       |
;           -------|--------------^- -----------------------|------^
;                  |              |                         |      |
;                  |          -------------- <------        v      |		Parameters : balance
;                --       E1 | balance : 50 <-----  |      ㅇㅇ----		body :
;               |            | withdraw ------    | |       |			(define (withdraw amount)
;               |            | deposit----  | |   | |        ----------------->	 (if (>=balance amount)
;               |            | dispatch   | |  -  | |				   (begin (set! balace (- balace amount))
;               |             -|-^----^---|-    | | |					balance)
;               |              | |    |   |     v | |				"Insufficient funds")))
;               |             -  |    |   |    ㅇㅇ |				(define (deposit amount)
;               |            |   |    |   v     |   |				 (set! balance (+ balance amount))
;               |	     v   |    |  ㅇㅇ-------				  balance)
;               |	    ㅇㅇ-     |		  |				(define (dispatch m)
;               |                     |		  v				 (cond ((eq? m 'withdraw) withdraw)
;             ㅇㅇ--------------------	parameters : amount				((eq? m 'deposit) deposit)
;	      |				body :						(else (error "Unkown request --MAKE-ACCOUNT" m))))
;	       -			 (if (>= balance amount)		disptach)
;		|				(begin (set! balance (- balance amount))
;		v					balance)
;	parameters :				"Insufficient funds"))
;       body :
;       dispatch
;			parameters : m							parameters : amount
;			body :								body :
;			(cond ((eq? m 'withdraw) withdraw)				(set! balance (+ balance amount))
;				((eq? m 'deposit) deposit)					balance)
;				(else (error "Unkown request --MAKE-ACCOUNT" m))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           --------------------------------------------------------
;          | make-account ----------------------------------        |
;          | acc---                                         |       |
;           -------|--------------^- -----------------------|------^
;                  |              |                         |      |
;                  |          --------------                v      |			Parameters : balance
;                --       E1 | balance : 50 <-----         ㅇㅇ----			body :
;               |            | withdraw ... |     |         |				(define (withdraw amount)
;               |            | deposit ...  |     |          ----------------->		 (if (>=balance amount)
;               |            | dispatch ... |     |  					   (begin (set! balace (- balace amount))
;               |             --------^-----      |  						balance)
;               |                     |           |  					"Insufficient funds")))
;               |                     |      --------------				(define (deposit amount)
;               |                     |     | m : deposit  <----			 (set! balance (+ balance amount))
;               |	              |      --------------	|			  balance)
;               |	              |		  		|			(define (dispatch m)
;               |                     |		  		|			 (cond ((eq? m 'withdraw) withdraw)
;             ㅇㅇ--------------------	(cond ((eq? m 'withdraw)|withdraw)		  ((eq? m 'deposit) deposit)
;	      |				((eq? m 'deposit) deposit)			  (else (error "Unkown request --MAKE-ACCOUNT"
;	       -			(else (error "Unkown request --MAKE-ACCOUNT"		m))))
;		|			 m))))			|			disptach)
;		v				 ---------------
;	parameters :				| amount : 40   |
;       body :					 ---------------
;       dispatch
;
;				(set! balance (+ balance amount))
;					balance)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           --------------------------------------------------------
;          | make-account ----------------------------------        |
;          | acc---                                         |       |
;           -------|--------------^- -----------------------|------^
;                  |              |                         |      |
;                  |          --------------                v      |			Parameters : balance
;                --       E1 | balance : 90 |              ㅇㅇ----			body :
;               |            | withdraw ... |               |				(define (withdraw amount)
;               |            | deposit ...  |                ----------------->		 (if (>=balance amount)
;               |            | dispatch ... |        					   (begin (set! balace (- balace amount))
;               |             --------^-----         						balance)
;               |                     |              					"Insufficient funds")))
;               |                     |							(define (deposit amount)
;               |                     |							 (set! balance (+ balance amount))
;               |	              |							  balance)
;               |	              |							(define (dispatch m)
;               |                     |							 (cond ((eq? m 'withdraw) withdraw)
;             ㅇㅇ--------------------							  ((eq? m 'deposit) deposit)
;	      |										  (else (error "Unkown request --MAKE-ACCOUNT"
;	       -										m))))
;		|									disptach)
;		v
;	parameters :
;       body :
;       dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           --------------------------------------------------------
;          | make-account ----------------------------------        |
;          | acc---                                         |       |
;           -------|--------------^- -----------------------|------^
;                  |              |                         |      |
;                  |          --------------                v      |			Parameters : balance
;                --       E1 | balance : 90 <-----         ㅇㅇ----			body :
;               |            | withdraw ... |     |         |				(define (withdraw amount)
;               |            | deposit ...  |     |          ----------------->		 (if (>=balance amount)
;               |            | dispatch ... |     |  					   (begin (set! balace (- balace amount))
;               |             --------^-----      |  						balance)
;               |                     |           |  					"Insufficient funds")))
;               |                     |      --------------				(define (deposit amount)
;               |                     |     | m : withdraw <----			 (set! balance (+ balance amount))
;               |	              |      --------------	|			  balance)
;               |	              |		  		|			(define (dispatch m)
;               |                     |		  		|			 (cond ((eq? m 'withdraw) withdraw)
;             ㅇㅇ--------------------	(cond ((eq? m 'withdraw)|withdraw)		  ((eq? m 'deposit) deposit)
;	      |				((eq? m 'deposit) deposit)			  (else (error "Unkown request --MAKE-ACCOUNT"
;	       -			(else (error "Unkown request --MAKE-ACCOUNT"		m))))
;		|			 m))))			|			disptach)
;		v				 ---------------
;	parameters :				| amount : 60   |
;       body :					 ---------------
;       dispatch
;
;				(if (>=balance amount)
;					(begin (set! balace (- balace amount))
;						balance)
;					"Insufficient funds")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           --------------------------------------------------------
;          | make-account ----------------------------------        |
;          | acc---                                         |       |
;           -------|--------------^- -----------------------|------^
;                  |              |                         |      |
;                  |          --------------                v      |			Parameters : balance
;                --       E1 | balance : 30 |              ㅇㅇ----			body :
;               |            | withdraw ... |               |				(define (withdraw amount)
;               |            | deposit ...  |                ----------------->		 (if (>=balance amount)
;               |            | dispatch ... |        					   (begin (set! balace (- balace amount))
;               |             --------^-----         						balance)
;               |                     |              					"Insufficient funds")))
;               |                     |							(define (deposit amount)
;               |                     |							 (set! balance (+ balance amount))
;               |	              |							  balance)
;               |	              |							(define (dispatch m)
;               |                     |							 (cond ((eq? m 'withdraw) withdraw)
;             ㅇㅇ--------------------							  ((eq? m 'deposit) deposit)
;	      |										  (else (error "Unkown request --MAKE-ACCOUNT"
;	       -										m))))
;		|									disptach)
;		v
;	parameters :
;       body :
;       dispatch
;;;;;;;;;;;;;;;;;;;;acc2가 추가되는데 ()안의 수치만 다르고 동일하다;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           --------------------------------------------------------
;          | make-account ----------------------------------        |
;          | acc(2)                                         |       |
;           -------|--------------^- -----------------------|------^
;                  |              |                         |      |
;                  |          -------------- <------        v      |		Parameters : balance
;                --       E1 |balance:50(100)<----  |      ㅇㅇ----		body :
;               |            | withdraw ------    | |       |			(define (withdraw amount)
;               |            | deposit----  | |   | |        ----------------->	 (if (>=balance amount)
;               |            | dispatch   | |  -  | |				   (begin (set! balace (- balace amount))
;               |             -|-^----^---|-    | | |					balance)
;               |              | |    |   |     v | |				"Insufficient funds")))
;               |             -  |    |   |    ㅇㅇ |				(define (deposit amount)
;               |            |   |    |   v     |   |				 (set! balance (+ balance amount))
;               |	     v   |    |  ㅇㅇ-------				  balance)
;               |	    ㅇㅇ-     |		  |				(define (dispatch m)
;               |                     |		  v				 (cond ((eq? m 'withdraw) withdraw)
;             ㅇㅇ--------------------	parameters : amount				((eq? m 'deposit) deposit)
;	      |				body :						(else (error "Unkown request --MAKE-ACCOUNT" m))))
;	       -			 (if (>= balance amount)		disptach)
;		|				(begin (set! balance (- balance amount))
;		v					balance)
;	parameters :				"Insufficient funds"))
;       body :
;       dispatch
;			parameters : m							parameters : amount
;			body :								body :
;			(cond ((eq? m 'withdraw) withdraw)				(set! balance (+ balance amount))
;				((eq? m 'deposit) deposit)					balance)
;				(else (error "Unkown request --MAKE-ACCOUNT" m))))