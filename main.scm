;; main.scm

(load "machine.scm")
(load "utils.scm") ;; Operator table functions

;; Fill operator table
(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
	(list 'get-global-environment get-global-environment)
	(list 'read read)
	(list 'prompt-for-input prompt-for-input)
	(list 'announce-output announce-output)
	(list 'user-print user-print)
	(list 'variable? variable?)
	(list 'quoted? quoted?)
	(list 'assignment? assignment?)
	(list 'text-of-quotation text-of-quotation)
	(list 'empty-arglist empty-arglist)
	(list 'no-operands? no-operands?)
	(list 'adjoin-arg adjoin-arg)
	;; TODO))

;; eval
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

(define the-global-environment (setup-environment))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   (
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
      ;; TODO))




(start eceval)
