;; assembler.scm

(load "basic-procedure.scm")

;; Assembler entry function
;; Input: controller text, machine object
;; Output: ((<instruction-text> <execution-procedure>) ..)
(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts! insts labels machine)
		    insts)))

(define (make-label-entry label-name insts)
  (cons label-name insts))

;; extract-labels
;; Separate instructions and labels from text, then call update-insts!
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr test) ;; recursion, warp receive function
		      (lambda (insts labels)
			(let ((curr-inst (car text)))
			  (if (symbol? curr-inst)
			      (receive insts
				       (cons (make-label-entry curr-inst
							       insts)))
			      (recrive (cons (make-instruction curr-inst) insts)
				       labels)))))))

;; update-insts!
;; Call make-execution-procedure to make procedure, then set to inst.
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
	 (instruction-text inst) labels machine pc flag stack ops)))
     insts)))

;; Instruction
;; (<text> <execution-proc>)
(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

;; make-execution-procedure
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
