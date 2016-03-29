;; basic-procedure.scm
;;
;; Require: stack.scm register.scm (load in machine.scm)
;;
;; Instruction:
;;
;; ('instruction-name ('register-name expression))
;;  ^ operation-exp: tagged with 'op here


;; Helpers
(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; Register
;; '(reg <register>)
(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

;; Const
;; '(const <value>)
(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

;; Label
;; '(label <label>)
(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

;; Operation
;; '((op <operator>) <operands>)
;; Example: '((op =) (reg b) (const 0))
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op exp)
  (cadr (car exp)))

(define (operation-exp-operands exp)
  (cdr exp))

;; Lookup operation in ops
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
	(cadr val)
	(error "Unknown operation --assemble" symbol))))

;; Deal with each operand of operation
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
	 (let ((c (constant-exp-value exp)))
	   (lambda () c)))
	((label-exp? exp)
	 (let ((insts
		(lookup-label labels (label-exp-label exp))))
	   (lambda () insts)))
	((register-exp? exp)
	 (let ((r (get-register machine
				(register-exp-reg exp))))
	   (lambda () (get-contents r))))
	(else
	 (error "Unknown expression type -- assemble" exp))))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs
	 (map (lambda (e)
		(make-primitive-exp e machine labels))
	      (operation-exp-operands exp))))
    (lambda ()
      ;; Explain (map (lambda (p) (p)) aprocs):
      ;; ((lambda (x) (x)) (+ 1 2)) => 3 is not applicable
      ;; ((lambda (x) (x)) '(lambda () 1)) => (lambda () 1) is not applicable
      ;; ((lambda (x) (x)) (lambda () 1)) => 1
      ;; ((lambda () 1)) => 1
      (apply op (map (lambda (p) (p)) aprocs)))))

;; ASSIGN
;; (assign <target-register> <value-expression>)
;; Set target register and advance pc
(define (make-assign inst machine labels operations pc)
  (let ((target
	 (get-register machine assign-reg-name inst))
	(value-exp
	 (assign-value-exp inst)))
    (let ((value-proc
	   (if (operation-exp? value-exp)
	       (make-operation-exp
		value-exp machine labels operations)
	       (make-primitive-exp
		(car value-exp) machine labels))))
      (lambda ()
	(set-contents! target (value-proc))
	(advance-pc pc)))))

(define (assign-reg-name inst) (cadr inst))

(define (assign-value-exp inst) (cddr inst))

;; TEST
;; '(test <condition>)
;; Set flag and advance pc
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
	(let ((condition-proc
	       (make-operation-exp condition machine labels operations)))
	  (lambda ()
	    (set-contents! flag (condition-proc))
	    (advance-pc)))
	(error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;; BRANCH
;; '(branch <dest(label)>)
;; Test flag, set pc to label content or advance pc
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
	(let ((insts (lookup-label labels (lable-exp-label dest))))
	  (lambda ()
	    (if (get-contents flag)
		(set-contents! pc insts)
		(advance-pc))))
	(error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;; GOTO
;; '(goto <dest(label/register)>)
;; Set pc to label content or register content
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts
		  (lookup-label labels (lable-exp-label dest))))
	     (lambda ()
	       (set-contents! pc insts))))
	  ((register-exp? dest)
	   (let ((reg
		  (get-register machine (register-exp-reg dest))))
	     (lambda ()
	       (set-contents! pc (get-contents reg)))))
	  (else (error "Bad GOTO instruction -- assemble" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; SAVE
;; '(save <register>)
;; Push register content on stack and advance pc
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;; RESTORE
;; '(restore <register>)
;; Pop from stack and set it to target register
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc))))

;; PERFORM
;; '(perform <action>)
;; Do action and advance pc
(define (make-perform inst machine labels operation pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
	(let ((action-proc
	       (make-operation-exp action machine labels operations)))
	  (lambda ()
	    (action-proc)
	    (advance-pc)))
	(error "Bad PERFORM instruction --assemble" inst))))

(define (perform-action inst)
  (cdr inst))
