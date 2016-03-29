;; main.scm

(load "machine.scm")
(load "utils.scm")

;; Fill opterator table
(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
	(list 'read read)
	;; TODO))

;; eval
;;
;; 7 registers:
;;
;; exp: expression to evaluate
;; env: evaluate environment
;; val: evaluate result
;; continue: used in recursion
;; proc: for compound procedure
;; argl: for compound procedure
;; unev: unevaluated expression for compound procedure

(define the-global-environment (setup-environment))

(define (get-global-environment) the-global-environment)

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
     
     
     )))



    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))

    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))

    (test (op definition) (reg exp))
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


(start eceval)
