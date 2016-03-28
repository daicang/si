;; machine.scm

(load "stack.scm")
(load "register.scm")

;; make-new-machine
(define (make-new-machine)
  ;; Construct an object, which contains:
  ;; 1. stack
  ;; 2. register table (has PC and FLAG initially)
  ;; 3. operate table
  ;; 4. instruction sequence
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '()))
    
    (let ((the-ops
	   ;; (('initialize-stack (lambda () (stack 'initialize))))
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))))

	  ;; register table:
	  ;; (('pc pc) ('flag flag))
	  ;;
	  ;; PC register
	  ;; 1. modified by "brach" and "goto" directly
	  ;; 2. added by any other instruction
	  ;; 
	  ;; FLAG register
	  ;; 1. set by "test" instruction
	  ;; 2. checked by "branch" instruction to
	  ;; determine jump or not
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "allocate-register: Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "lookup-register: Unknown register:" name))))
      
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		;; Execute instructions
		;; instruction-execution-proc is cdr,
		;; so this is ((cdr (car insts)))
		;;
		;; Where is advance-pc?
		((instruction-execution-proc (car insts)))
		(execute)))))

      ;; What's the difference between
      ;; the-instruction-sequence and ops?
      (define (dispatch msg)
	(cond ((eq? msg 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      
	      ((eq? msg 'install-instruction-sequence)
	       (lambda (seq) (set! the-instruction-sequence seq)))
	      
	      ((eq? msg 'allocate-register) allocate-register)
	      
	      ((eq? msg 'get-register) lookup-registre)
	      
	      ((eq? msg 'install-operations)
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      
	      ((eq? msg 'stack) stack)
	      
	      ((eq? msg 'operations) the-ops)
	      
	      (else
	       (error "make-new-machine: Unknown request: " msg))))
      dispatch)))

(define (start machine) (machine 'start))

(define (get-register machine reg) ((machine 'get-register) reg))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)))

;; Example:
;;
;; (make-machine
;;  ;; registers
;;  '(a b c)
;;  ;; library
;;  (list (list 'rem remainder) (list '= =))
;;  ;; code (not assembled?)
;;  '(test-b
;;    (test (op =) (reg b) (const 0))
;;    (branch (label gcd-done))
;;    (assign t (op rem) (reg a) (reg b))
;;    (assign a (reg b))
;;    (assign b (reg t))
;;    (goto (label test-b))
;;    gcd-done))
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-names)
		((machine 'allocate-register)))
	      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
