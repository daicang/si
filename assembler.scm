;; assembler.scm
;; Function assemble called by make-machine in machine.scm

;; Require "make-<instruction>" procedures
(load "basic-procedure.scm")

;; Instruction structure
;; (<text> <execution-proc>)
(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

;; Assembler entry function
;; Input:
;; controller text, machine object
;;
;; Output:
;; list of machine instructions, each
;; instruct containes its execution procedure
;; ((<instruction-text> <execution-procedure>) ..)
(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    ;; Call update-insts!
		    ;; after finishing extract-labels
		    (update-insts! insts labels machine)
		    insts)))


;; extract-labels
;; The machine instruction contains:
;; 1. the instruction
;; 2. corresponding execution procedure
;; (2) is set in update-insts!
(define (extract-labels text receive)
  (if (null? text)
      ;; Initialize instruction list / label list
      (receive '() '())
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((curr-inst (car text)))
			  (if (symbol? curr-inst)
			      ;; Label, add to label list (contains duplicated
			      ;; instructions?)
			      ;;
			      ;; Example:
			      ;;
			      ;; code:
			      ;;
			      ;; label1
			      ;;  (inst1)
			      ;; label2
			      ;;  (inst2)
			      ;;
			      ;; label list:
			      ;;
			      ;; ((label1 ((inst1) (inst2)))
			      ;;  (label2 ((inst2))))
			      ;;
			      ;; ((<label-name> <instructions>) ...)
			      (receive
			       insts
			       (cons (make-label-entry curr-inst insts)))
			      ;; Instruction, add to instruction list
			      ;; ((<instruction-text>) ...)
			      (receive
			       (cons (make-instruction curr-inst) insts)
			       labels)))))))

(define (make-instruction text)
  (cons text '()))

(define (make-label-entry label-name insts)
  (cons label-name insts))

;; update-insts!
;; Set (cdr <instruction>)
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(stack (machine 'stack))
	(ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! ;; set-cdr!
	inst
	(make-execution-procedure
	 (instruction-text inst) labels machine pc flag stack ops)))
     insts)))


;; make-execution-procedure
;;
;; Dispatch 7 basic instructions:
;; ASSIGN
;; TEST
;; BRANCH
;; GOTO
;; SAVE
;; RESTORE
;; PERFORM
(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops flag pc))
	((eq? (car inst) 'branch)
	 (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto)
	 (make-goto inst machine stack pc))
	((eq? (car inst) 'save)
	 (make-save inst machine stack pc))
	((eq? (car inst) 'restore)
	 (make-restore inst machine stack pc))
	((eq? (car inst) 'perform)
	 (make-perform inst machine labels ops pc))
	(else
	 (error "make-execution-procedure: Unknown instruction type: "
		inst))))
