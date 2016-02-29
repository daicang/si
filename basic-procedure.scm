;; basic-procedure.scm
;;
;; machine.scm -> stack.scm
;;            |-> basic-procedure.scm -> register.scm
;;
;; Instruction: TODO: is it true? see (operation-exp?)
;; ('instruction-name ('register-name expression))

(load "register.scm")

;; Helpers
(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; assign
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

;; test
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
	(make-operation-exp
	 condition machine labels operations)
	(lambda ()
	  (set-contents! flag (condition-proc))))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;; branch
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
	(let ((insts (lookup-label labels (lable-exp-label dest))))
	  (lambda ()
	    (if (get-contents flag)
		(set-contents! pc insts)
		(advance-pc))))
	(error "Bad BRANCH instruction -- assemble" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;; goto
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts
		  lookup-label labels (lable-exp-label dest)))))
	  ((register-exp? dest)
	   (let ((reg
		  (get-register machine
				(register-exp-reg dest))))
	     (lambda ()
	       (set-contents! pc (get-contents reg)))))
	  (else (error "Bad GOTO instruction -- assemble" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; stack instructions
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

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

