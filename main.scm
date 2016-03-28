;; main.scm
;; TODO

(define prompt "ss> ")

(define (main-loop)
  (let ((i (read)))
    (display i)))

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

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(
     read-eval-print-loop
     ;; TODO
     )))

(define eceval-operations
  (list (list 'self-evaluating? self-evaluating)
	))


(define (eval exp env)
  (define (eval-dispatch exp)
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

    (goto (label unknown-expression-type))))

(define the-global-environment (setup-environment))

(start eceval)
