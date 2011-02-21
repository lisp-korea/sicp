;ex 3.9-1
;           --------------------
;          | factorial :        |
;           --------------------
;            |   ^
;             -  |
;              |  -
;              v   |
;             しし-
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
;             しし-        しし---
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
;                --       E1 |initial-amount:100|   しし----
;               |     E2      --------^---------    |
;               |      |              |             |
;               |    --               |             |
;               |   |    -----------  |             |
;               |    -->|balance:100|-              V
;               |        ----^------         Parameters : initial-amount
;               |            |               body :
;             しし-----------                 ((lambda (balance)
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
;                --       E1 |initial-amount:100|   しし----
;               |     E2      --------^---------    |
;               |      |              |             |
;               |    --               |             |
;               |   |    -----------  |             |
;               |    -->|balance:100|-              V
;               |        ----^---^--					Parameters : initial-amount
;               |            |   |					body :
;             しし-----------    |					 ((lambda (balance)
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
;                --       E1 |initial-amount:100|   しし----
;               |     E2      --------^---------    |
;               |      |              |             |
;               |    --               |             |
;               |   |     ----------  |             |
;               |    --> |balance:50|-              V
;               |         ---^------         Parameters : initial-amount
;               |            |               body :
;             しし-----------                 ((lambda (balance)
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
;               |      |              |              |      |    |                  |           しし--
;               |    --               |             しし----      ------------------             |
;               |   |    ----------   |              |             Parameters : initial-amount<--
;               |    -->|balance:50|--               V             body :
;               |        ---^------  Parameters : amount                     (lambda (balance)
;               |           |        body :                                   (lambda (amount)
;             しし----------          (if (>= balance amount)                  (if (>=balance amount)
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