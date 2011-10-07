;; 5.2 A Register-Machine Simulator

;; (make-machine <register-names> <operations> <controller>)
;; (set-register-contents! <machine-model> <register-name> <value>)
;; (get-register-contents <machine-model> <register-name>)
;; (start <machine-model>)

;; (define gcd-machine
;;   (make-machine
;;    '(a b t)
;;    (list (list 'rem remainder) (list '= =))
;;    '(test-b
;;        (test (op =) (reg b) (const 0))
;;        (branch (label gcd-done))
;;        (assign t (op rem) (reg a) (reg b))
;;        (assign a (reg b))
;;        (assign b (reg t))
;;        (goto (label test-b))
;;      gcd-done)))

;; (set-register-contents! gcd-machine 'a 206)
;; done
;; (set-register-contents! gcd-machine 'b 40)
;; done
;; (start gcd-machine)
;; done
;; (get-register-contents gcd-machine 'a)
;; 2

;; ex 5.7
;; where is my simulator? let me see.

;; 5.2.1 The Machine Model

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
	(for-each (lambda (register-name)
				((machine 'allocate-register) register-name))
			  register-names)
	((machine 'install-operations) ops)
	((machine 'install-instruction-sequence)
	 (assemble controller-text machine))
	machine))

;; Registers

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
	dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; The Stack

(define (make-stack)
  (let ((s '()))
	(define (push x)
	  (set! s (cons x s)))
	(define (pop)
	  (if (null? s)
		  (error "Empty stack -- POP")
		  (let ((top (car s)))
			(set! s (cdr s))
			top)))
	(define (initialize)
	  (set! s '())
	  'done)
	(define (dispatch message)
	  (cond ((eq? message 'push) push)
			((eq? message 'pop) (pop))
			((eq? message 'initialize) (initialize))
			(else (error "Unknown request -- STACK"
						 message))))
	dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))


;; The basic machine

(define (make-new-machine)
  (let ((pc (make-register 'pc))
		(flag (make-register 'flag))
		(stack (make-stack))
		(the-instruction-sequence '()))
	(let ((the-ops
		   (list (list 'initialize-stack
					   (lambda () (stack 'initialize)))))
		  (register-table
		   (list (list 'pc pc)
				 (list 'flag flag))))
	  (define (allocate-register name)
		(if (assoc name register-table)
			(error "Multiply defined register: " name)
			(set! register-table
				  (cons (list name (make-register name))
						register-table)))
		'register-allocated)
	  (define (lookup-register name)
		(let ((val (assoc name register-table)))
		  (if val
			  (cadr val)
			  (error "Unknown register:" name))))
	  (define (execute)
		(let ((insts (get-contents pc)))
		  (if (null? insts)
			  'done
			  (begin
				((instruction-execution-proc (car insts)))
				(execute)))))
	  (define (dispatch message)
		(cond ((eq? message 'start)
			   (set-contents! pc the-instruction-sequence)
			   (execute))
			  ((eq? message 'install-instruction-sequence)
			   (lambda (seq) (set! the-instruction-sequence seq)))
			  ((eq? message 'allocate-register) allocate-register)
			  ((eq? message 'get-register) lookup-register)
			  ((eq? message 'install-operations)
			   (lambda (ops)
				 (set! the-ops (append the-ops ops))))
			  ((eq? message 'stack) stack)
			  ((eq? message 'operations) the-ops)
			  (else (error "Unknown request -- MACHINE" message))))
	  dispatch)))


(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))


;; 5.2.2 The Assembler

(define (assemble controller-text machine)
  (extract-labels controller-text
				  (lambda (insts labels)
					(update-insts! insts labels machine)
					insts)))

(define (extract-labels text receive)
  (if (null? text)
	  (receive '() '())
	  (extract-labels (cdr text)
					  (lambda (insts labels)
						(let ((next-inst (car text)))
						  (if (symbol? next-inst)
							  (receive insts
									   (cons (make-label-entry next-inst
															   insts)
											 labels))
							  (receive (cons (make-instruction next-inst)
											 insts)
									   labels)))))))


(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
		(flag (get-register machine 'flag))
		(stack (machine 'stack))
		(ops (machine 'operations)))
	(for-each
	 (lambda (inst)
	   (set-instruction-execution-proc!
		inst
		(make-execution-procedure
		 (instruction-text inst) labels machine
		 pc flag stack ops)))
	 insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
	(if val
		(cdr val)
		(error "Undefined label -- ASSEMBLE" label-name))))


;; ex 5.8

;; start
;;   (goto (label here))
;; here
;;   (assign a (const 3))
;;   (goto (label there))
;; here
;;   (assign a (const 4))
;;   (goto (label there))
;; there


;; the contents of register a will be 3.

(define (extract-labels-fix text receive)
  (if (null? text)
	  (receive '() '())
	  (extract-labels (cdr text)
					  (lambda (insts labels)
						(let ((next-inst (car text)))
						  (if (symbol? next-inst)
							  (if (null? (assoc next-inst labels))
								  (receive insts
										   (cons (make-label-entry next-inst
																   insts)
												 labels))
								  (error "Multiply defined label" next-inst))
							  (receive (cons (make-instruction next-inst)
											 insts)
									   labels)))))))


;; 5.2.3 Generating Execution Procedures for Instructions

(define (make-execution-procedure inst labels machine
								  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
		 (make-assign inst machine labels ops pc))
		((eq? (car inst) 'test)
		 (make-test inst machine labels ops flag pc))
		((eq? (car inst) 'branch)
		 (make-branch inst machine labels flag pc))
		((eq? (car inst) 'goto)
		 (make-goto inst machine labels pc))
		((eq? (car inst) 'save)
		 (make-save inst machine stack pc))
		((eq? (car inst) 'restore)
		 (make-restore inst machine stack pc))
		((eq? (car inst) 'perform)
		 (make-perform inst machine labels ops pc))
		(else (error "Unknown instruction type -- ASSEMBLE" inst))))

;; assign instructions

(define (make-assign inst machine labels operations pc)
  (let ((target
		 (get-register machine (assign-reg-name inst)))
		(value-exp (assign-value-exp inst)))
	(let ((value-proc
		   (if (operation-exp? value-exp)
			   (make-operation-exp
				value-exp machine labels operations)
			   (make-primitive-exp
				(car value-exp) machine labels))))
	  (lambda ()
		(set-contents! target (value-proc))
		(advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; test, branch, goto instructions

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
	(if (operation-exp? condition)
		(let ((condition-proc
			   (make-operation-exp
				condition machine labels operations)))
		  (lambda ()
			(set-contents! flag (condition-proc))
			(advance-pc pc)))
		(error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
	(if (label-exp? dest)
		(let ((insts
			   (lookup-label labels (label-exp-label dest))))
		  (lambda ()
			(if (get-contents flag)
				(set-contents! pc insts)
				(advance-pc pc))))
		(error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))
		
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
	(cond ((label-exp? dest)
		   (let ((insts
				  (lookup-label labels
								(label-exp-label dest))))
			 (lambda () (set-contents! pc insts))))
		  ((register-exp? dest)
		   (let ((reg
				  (get-register machine
								(register-exp-reg dest))))
			 (lambda ()
			   (set-contents! pc (get-contents reg)))))
		  (else
		   (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; Other instructions

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
						   (stack-inst-reg-name inst))))
	(lambda ()
	  (push stack (get-contents reg))
	  (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
						   (stack-inst-reg-name inst))))
	(lambda ()
	  (set-contents! reg (pop stack))
	  (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
	(if (operation-exp? action)
		(let ((action-proc
			   (make-operation-exp
				action machine labels operations)))
		  (lambda ()
			(action-proc)
			(advance-pc pc)))
		(error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;; Execution procedures for subexpressions

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
		 (let ((c (constant-exp-value exp)))
		   (lambda () c)))
		((label-exp? exp)
		 (let ((insts
				(lookup-label labels
							  (label-exp-label exp))))
		   (lambda () insts)))
		((register-exp? exp)
		 (let ((r (get-register machine
								(register-exp-reg exp))))
		   (lambda () (get-contents r))))
		(else
		 (error "Unknown expression type -- ASSEMBLE" exp))))

(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (car exp) tag)
	  #f))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
		(aprocs
		 (map (lambda (e)
				(make-primitive-exp e machine labels))
			  (operation-exp-operands exp))))
	(lambda ()
	  (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
	(if val
		(cadr val)
		(error "Unknown operation -- ASSEMBLE" symbol))))

;; test

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

(set-register-contents! gcd-machine 'a 206)
;; done
(set-register-contents! gcd-machine 'b 40)
;; done
(start gcd-machine)
;; done
(get-register-contents gcd-machine 'a)
;; 2


;; ex 5.9

(define (make-primitive-exp-fix exp machine labels)
  (cond ((constant-exp? exp)
		 (let ((c (constant-exp-value exp)))
		   (lambda () c)))
		;; ((label-exp? exp)
		;;  (let ((insts
		;; 		(lookup-label labels
		;; 					  (label-exp-label exp))))
		;;    (lambda () insts)))
		((register-exp? exp)
		 (let ((r (get-register machine
								(register-exp-reg exp))))
		   (lambda () (get-contents r))))
		(else
		 (error "Unknown expression type -- ASSEMBLE" exp))))

;; ex 5.10

;; (assign2 r1 r2 <exp1> <exp2>)

(define (make-assign2 inst machine labels operations pc)
  (let ((t1 (get-register machine (assign-first-reg-name inst)))
		(t2 (get-register machine (assign-second-reg-name inst)))
		(v1 (assign-first-value-exp inst))
		(v2 (assign second-value-exp inst)))
	(define (exp-to-proc exp)
	  (if (operation-exp? exp)
		  (make-operation-exp
		   exp machine labels operations)
		  (make-primitive-exp
		   (car exp) machine labels)))
	(let ((v1-proc (exp-to-proc v1)
		  (v2-proc (exp-to-proc v2))))
	  (lambda ()
		(set-contents! t1 (v1-proc))
		(set-contents! t2 (v2-proc))
		(advance-pc)))))

(define (assign-first-reg-name inst)
  (cadr inst))
(define (assign-second-reg-name inst)
  (caddr inst))
(define (assign-first-value-exp inst)
  (cadddr inst))
(define (assign-second-value-exp inst)
  (cddddr inst))


;; ex 5.11

;; a. save unnecessary operation between (restore n) ... (store n) if n isn't changed in section.

;; b.

(define (make-save-5-11-b inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
	(let ((reg (get-register machine reg-name)))
	  (lambda ()
		(push stack (cons reg-name (get-contents reg)))
		(advance-pc pc))))

(define (make-restore-5-11-b inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
	(let ((reg (get-register machine reg-name)))
	  (let ((content (pop stack)))
		(if (eq? reg-name (car content))
			(lambda ()
			  (set-contents! reg (cdr content))
			  (advance-pc pc)))))))


;; c.


(define (make-new-machine)
  (let ((pc (make-register 'pc))
		(flag (make-register 'flag))
		(stack-table '())
		(the-instruction-sequence '()))
	(let ((register-table
		   (list (list 'pc pc)
				 (list 'flag flag))))
	  (let ((the-ops
			 (list (list 'initialize-stack
						 (lambda ()
						   (for-each (lambda (reg)
									   (cons (list (car reg) (make-stack))
											 stack-table))
									 (register-table)))))))
		(define (allocate-register name)
		  (if (assoc name register-table)
			  (error "Multiply defined register: " name)
			  (set! register-table
					(cons (list name (make-register name))
						  register-table)))
		  'register-allocated)
		(define (lookup-register name)
		  (let ((val (assoc name register-table)))
			(if val
				(cadr val)
				(error "Unknown register:" name))))
		(define (lookup-stack name)
		  (let ((stack (assoc name stack-table)))
			(if stack
				(cadr stack)
				(error "Unknown stack:" name))))
		(define (execute)
		  (let ((insts (get-contents pc)))
			(if (null? insts)
			  'done
			  (begin
				((instruction-execution-proc (car insts)))
				(execute)))))
		(define (dispatch message)
		  (cond ((eq? message 'start)
				 (set-contents! pc the-instruction-sequence)
				 (execute))
				((eq? message 'install-instruction-sequence)
			   (lambda (seq) (set! the-instruction-sequence seq)))
				((eq? message 'allocate-register) allocate-register)
				((eq? message 'get-register) lookup-register)
				((eq? message 'install-operations)
				 (lambda (ops)
				 (set! the-ops (append the-ops ops))))
				((eq? message 'get-stack) lookup-stack)
				((eq? message 'operations) the-ops)
				(else (error "Unknown request -- MACHINE" message))))
		dispatch)))
  

(define (make-save-5-11-c inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
	(let ((stack ((machine 'get-stack) reg-name)))
	  (let ((reg (get-register machine reg-name)))
		(lambda ()
		  (push stack get-contents reg)
		  (advance-pc pc))))))

(define (make-restore-5-11-c inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
	(let ((stack ((machine 'get-stack) reg-name)))
	  (let ((reg (get-register machine reg-name)))
		(if (eq? reg-name (car content))
			(lambda ()
			  (set-contents! reg (pop stack))
			  (advance-pc pc)))))))


;; ex 5.12

(define (make-new-machine-ex-5-12)
  (let ((pc (make-register 'pc))
		(flag (make-register 'flag))
		(stack (make-stack))
		(the-instruction-sequence '())

		(instruction-log '())
		(entry-point-log '())
		(register-stack-log '())
		(register-source-log '()))
	(let ((the-ops
		   (list (list 'initialize-stack
					   (lambda () (stack 'initialize)))))
		  (register-table
		   (list (list 'pc pc)
				 (list 'flag flag))))
	  (define (allocate-register name)
		(if (assoc name register-table)
			(error "Multiply defined register: " name)
			(set! register-table
				  (cons (list name (make-register name))
						register-table)))
		'register-allocated)
	  (define (lookup-register name)
		(let ((val (assoc name register-table)))
		  (if val
			  (cadr val)
			  (error "Unknown register:" name))))
	  (define (execute)
		(let ((insts (get-contents pc)))
		  (if (null? insts)
			  'done
			  (begin
				((instruction-execution-proc (car insts)))
				(execute)))))
	  (define (dispatch message)
		(cond ((eq? message 'start)
			   (set-contents! pc the-instruction-sequence)
			   (execute))
			  ((eq? message 'install-instruction-sequence)
			   (lambda (seq) (set! the-instruction-sequence seq)))
			  ((eq? message 'allocate-register) allocate-register)
			  ((eq? message 'get-register) lookup-register)
			  ((eq? message 'install-operations)
			   (lambda (ops)
				 (set! the-ops (append the-ops ops))))
			  ((eq? message 'stack) stack)
			  ((eq? message 'operations) the-ops)

			  ;; getting log
			  ((eq? message 'get-instruction-log) instruction-log)
			  ((eq? message 'get-entry-point-log) entry-point-log)
			  ((eq? message 'get-register-stack-log) register-stack-log)
			  ((eq? message 'get-register-source-log) register-source-log)
			  
			  ;; logging information
			  ((eq? message 'log-instruction)
			   (lambda (inst)
				 (let ((entry (instruction-text inst)
							  instruction-log))
				   (if entry
					   '()
					   (set! instruction-log
							 (cons (list (instruction-text inst) 'occuered)
								   instruction-log))))))

			  ((eq? message 'log-entry-point)
			   (lambda (inst)
				 (let ((reg (cadadr inst)))
				   (let ((entry (assoc reg entry-point-log)))
					 (if entry
						 '()
						 (set! entry-point-log
							   (cons (list reg 'point)
									 entry-point-log)))))))

			  ((eq? message 'log-register-stack)
			   (lambda (inst)
				 (let ((reg (cadr inst)))
				   (let ((entry (assoc reg register-stack-log)))
					 (if entry
						 '()
						 (set! register-stack-log
							   (cons (list reg 'stack)
									 register-stack-log)))))))

			  ((eq? message 'log-register-source)
			   (lambda (inst)
				 (let ((reg (cadr inst))
					   (src (caddr inst)))
				   (let ((key (cons reg src)))
					 (let ((entry (assoc key register-source-log)))
					   (if entry
						   '()
						   (set! register-source-log
								 (cons (list key '())
									   register-source-log))))))))
			  (else (error "Unknown request -- MACHINE" message))))
	  dispatch))))

;; instruction
(define (make-execution-procedure-ex-5-12 inst labels machine
										  pc flag stack ops)
  ((machine 'log-instruction) inst)
  (cond ((eq? (car inst) 'assign)
		 (make-assign inst machine labels ops pc))
		((eq? (car inst) 'test)
		 (make-test inst machine labels ops flag pc))
		((eq? (car inst) 'branch)
		 (make-branch inst machine labels flag pc))
		((eq? (car inst) 'goto)
		 (make-goto inst machine labels pc))
		((eq? (car inst) 'save)
		 (make-save inst machine stack pc))
		((eq? (car inst) 'restore)
		 (make-restore inst machine stack pc))
		((eq? (car inst) 'perform)
		 (make-perform inst machine labels ops pc))
		(else (error "Unknown instruction type -- ASSEMBLE" inst))))


  ;; assign instructions
(define (make-assign-ex-5-12 inst machine labels operations pc)
  (let ((target
		 (get-register machine (assign-reg-name inst)))
		(value-exp (assign-value-exp inst)))
	(let ((value-proc
		   (if (operation-exp? value-exp)
			   (make-operation-exp
				value-exp machine labels operations)
			   (make-primitive-exp
				(car value-exp) machine labels))))
	  (lambda ()
		(set-contents! target (value-proc))
		((machine 'log-register-source) inst)
		(advance-pc pc)))))
;; goto		
(define (make-goto-ex-5-12 inst machine labels pc)
  (let ((dest (goto-dest inst)))
	(cond ((label-exp? dest)
		   (let ((insts
				  (lookup-label labels
								(label-exp-label dest))))
			 (lambda () (set-contents! pc insts))))
		  ((register-exp? dest)
		   (let ((reg
				  (get-register machine
								(register-exp-reg dest))))
			 (lambda ()
			   ((machine 'log-entry-point) inst)
			   (set-contents! pc (get-contents reg)))))
		  (else
		   (error "Bad GOTO instruction -- ASSEMBLE" inst)))))


;; stack
(define (make-save-ex-5-12 inst machine stack pc)
  (let ((reg (get-register machine
						   (stack-inst-reg-name inst))))
	(lambda ()
	  (push stack (get-contents reg))
	  ((machine 'log-register-stack) inst)
	  (advance-pc pc))))

(define (make-restore-ex-5-12 inst machine stack pc)
  (let ((reg (get-register machine
						   (stack-inst-reg-name inst))))
	(lambda ()
	  (set-contents! reg (pop stack))
	  ((machine 'log-register-stack) inst)
	  (advance-pc pc))))


;; ex 5.13


(define (make-machine-ex-5-13 ops controller-text)
  (let ((machine (make-new-machine)))
	((machine 'install-operations) ops)
	((machine 'install-instruction-sequence)
	 (assemble controller-text machine))
	machine))

(define (make-new-machine-ex-5-13)
  (let ((pc (make-register 'pc))
		(flag (make-register 'flag))
		(stack (make-stack))
		(the-instruction-sequence '()))
	(let ((the-ops
		   (list (list 'initialize-stack
					   (lambda () (stack 'initialize)))))
		  (register-table
		   (list (list 'pc pc)
				 (list 'flag flag))))
	  (define (allocate-register name)
		(if (assoc name register-table)
			(error "Multiply defined register: " name)
			(set! register-table
				  (cons (list name (make-register name))
						register-table)))
		'register-allocated)
	  (define (lookup-register name)
		(let ((val (assoc name register-table)))
		  (if val
			  (cadr val)
			  (allocate-register name))))
	  (define (execute)
		(let ((insts (get-contents pc)))
		  (if (null? insts)
			  'done
			  (begin
				((instruction-execution-proc (car insts)))
				(execute)))))
	  (define (dispatch message)
		(cond ((eq? message 'start)
			   (set-contents! pc the-instruction-sequence)
			   (execute))
			  ((eq? message 'install-instruction-sequence)
			   (lambda (seq) (set! the-instruction-sequence seq)))
			  ((eq? message 'allocate-register) allocate-register)
			  ((eq? message 'get-register) lookup-register)
			  ((eq? message 'install-operations)
			   (lambda (ops)
				 (set! the-ops (append the-ops ops))))
			  ((eq? message 'stack) stack)
			  ((eq? message 'operations) the-ops)
			  (else (error "Unknown request -- MACHINE" message))))
	  dispatch)))

