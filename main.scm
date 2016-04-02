;; main.scm

;; Inport meta-circular evaluater functions
(load "environment.scm")
(load "io.scm")
(load "procedure.scm")
(load "utils.scm")

;; Register machine
(load "machine.scm")


;; Initialize environment
(define the-global-environment (setup-environment))

;; Fill operator table
(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
	;; REPL
	(list 'get-global-environment get-global-environment)
	(list 'read read)
	(list 'prompt-for-input prompt-for-input)
	(list 'announce-output announce-output)
	(list 'user-print user-print)
	;; Type
	(list 'variable? variable?)
	(list 'quoted? quoted?)
	(list 'assignment? assignment?)
	(list 'text-of-quotation text-of-quotation)
	(list 'empty-arglist empty-arglist)
	(list 'no-operands? no-operands?)
	(list 'adjoin-arg adjoin-arg)
	(list 'apply-primitive-procedure apply-primitive-procedure)
	(list 'primitive-procedure? primitive-procedure?)
	(list 'compound-procedure? compound-procedure?)
	(list 'procedure-parameters procedure-parameters)
	(list 'procedure-environment procedure-environment)
	(list 'procedure-body procedure-body)
       	(list 'extend-environment extend-environment)
	(list 'first-exp first-exp)
	(list 'last-exp? last-exp?)
	(list 'rest-exps rest-exps)
	(list 'if-predicate if-predicate)
	(list 'if-alternative if-alternative)
	(list 'if-consequent if-consequent)
	;; Assignment
	(list 'assignment-variable assignment-variable)
	(list 'assignment-value assignment-value)
	(list 'set-variable-value! set-variable-value!)
	;; Defination
	(list 'defination-variable defination-variable)
	(list 'defination-value defination-value)
	(list 'define-variable! define-variable!)
	))


;; Eval
;;
;; 7 registers:
;;
;; exp: expression to evaluate
;; env: evaluate environment
;; val: evaluate result
;; continue: used in recursion
;; proc: procedure body for compound procedure
;; argl: argument list for compound procedure
;; unev: unevaluated expression for compound procedure

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(
    read-eval-print-loop
      (perform (op initialize-stack))
      (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (label eval-dispatch))
    print-result
      (perform (op announce-output) (const ";;; EC-Eval value:"))
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
    ;; Main evaluator
    eval-dispatch
      (test (op self-evaluating?) (reg exp))
      (branch (label ev-self-eval))
      (test (op variable?) (reg exp))
      (branch (label ev-variable))
      (test (op quoted?) (reg exp))
      (branch (label ev-quoted))
      (test (op assignment?) (reg exp))
      (branch (label ev-assignment))
      (test (op definition?) (reg exp))
      (branch (label ev-definition))
      (test (op if?) (reg exp))
      (branch (label ev-if))
      (test (op lambda?) (reg exp))
      (branch (label ev-lambda))
      (test (op begin?) (reg exp))
      (branch (label ev-begin))
      (test (op application?) (reg exp))
      (branch (label ev-application))
      (goto (label unknown-expression-type))
    ev-self-eval
      (assign val (reg exp))
      (goto (reg continue))
    ev-variable
      (assign val (op lookup-variable-value) (reg exp))
      (goto (reg continue))
    ev-quoted
      (assign val (op text-of-quotation) (reg exp))
      (goto (reg continue))
    ev-lambda
      (assign unev (op lambda-parameters) (reg exp))
      (assign exp (op lambda-body) (reg exp))
      (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
      (goto (reg continue))
    ev-application
      (save continue)
      (save env)
      (assign unev (op operands) (reg exp))
      (save unev) ;; Operands
      (assign exp (op operator) (reg exp))
      (assign continue (label ev-appl-did-operator)) ;; 2. Then evaluate args
      (goto (label eval-dispatch)) ;; 1. Evaluate the operator
    ev-appl-did-operator
      (restore unev) ;; Operands
      (restore env)
      (assign argl (op empty-arglist))
      (assign proc (reg val)) ;; Operator (after eval-dispatch)
      (test (op no-operands?) (reg unev))
      (branch (label apply-dispatch))
      (save proc)
    ev-appl-operand-loop ;; Loop body, prepare to evaluate an unevaluated argument
      (save argl) ;; save 1
      (assign exp (op first-operand) (reg unev)) ;; This argument
      (test (last-operand?) (reg unev))
      (branch (label ev-appl-last-arg))
      (save env) ;; save 2
      (save unev) ;; save 3
      (assign continue (label ev-appl-accumulate-arg)) ;; Continue other args
      (goto (label eval-dispatch)) ;; After evaluate this one
    ev-appl-accumulate-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl)) ;; Append to arl
      (assign unev (op rest-operands) (reg unev)) ;; Remove evaluated arg
      (goto (label ev-appl-operand-loop))
    ev-appl-last-arg
      (assign continue (label ev-appl-accum-last-arg))
      (goto (label eval-dispatch))
    ev-appl-accum-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (goto (label apply-dispatch))
    apply-dispatch ;; Correspend to apply
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-apply))
      (test (op compound-procedure?) (reg proc))
      (branch (label compound-apply))
      (goto (label unknown-procedure-type))
    primitive-apply
      (assign val (op apply-primitive-procedure)
	      (reg proc)
	      (reg argl))
      (restore continue)
      (goto (reg continue))
    compound-apply
      (assign unev (op procedure-parameters)) ;; Parameters
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment)
	      (reg unev)
	      (reg argl)
	      (reg env))
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))
    ev-begin
      (assign unev (op begin-actions) (reg exp))
      (save continue)
      (goto (label ev-sequence))
    ;; ev-sequence: evaluate current exp, 
    ;; if it's the last, goto ev-sequece-last-exp instead.
    ev-sequence 
      (assign exp (op first-exp))
      (test (op last-exp?) (reg unev))
      (branch (label ev-sequence-last-exp))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))
    ;; Tail recursion: we call eval directly, not saving anything on stack
    ;; No save/load env and unev 
    ;; 
    ;; Question: if we s/l env between exps, is environment-changing 
    ;; assignments unavailable?
    ev-sequence-last-exp
      (restore continue)
      (goto (label eval-dispatch))
    ev-if
      (save exp)
      (save env)
      (save continue)
      (assign continue (label ev-if-decide))
      (assign exp (op if-predicate) (reg exp))
      (goto (label eval-dispatch))
    ev-if-decide
      (restore continue)
      (restore env)
      (restore exp)
      (test (op true?) (reg val))
      (branch (label ev-if-consequent))
    ev-if-alternative
      (assign exp (op if-alternative) (reg exp))
      (goto (label eval-dispatch))
    ev-if-consequent
      (assign exp (op if-consequent) (reg exp))
      (goto (label eval-dispatch))
    ;; Assignment and defination
    ev-assignment
       (assign unev (op assignment-variable) (reg exp))
       (save unev) ;; unev <= variable
       (assign exp (op assignment-value) (reg exp))
       (save env) ;; exp <= value
       (save continue)
       (assign continue (label ev-assignment-1))
       (goto (label eval-dispatch))
     ev-assignment-1
       (restore continue)
       (restore env)
       (restore unev)
       (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
       (assign val (const ok))
       (goto (reg continue))
     ev-definition
       (assign unev (op defination-variable) (reg exp))
       (save unev)
       (assign exp (op defination-value) (reg exp))
       (save env)
       (save continue)
       (assign continue (label ev-definition-1))
       (goto (label eval-dispatch))
     ev-definition-1
       (restore continue)
       (restore env)
       (restore unev)
       (perform (op define-variable!) (reg unev) (reg val) (reg env))
       (assign val (const ok))
       (goto (reg continue))
     ;; Error handling
     unknown-procedure-type
       (assign val (const unknown-procedure-type-error))
       (goto (label signal-error))
     unknown-expression-type
       (assign val (const unknown-expression-type-error))
       (goto (label signal-error))
     signal-error
       (perform (op user-print) (reg val))
       (goto (label read-eval-print-loop)))))


(start eceval)
