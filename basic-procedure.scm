;; basic-procedure.scm
;;
;; machine.scm -> stack.scm
;;            |-> basic-procedure.scm -> register.scm

(load "register.scm")



;; Instruction structure:
;; ('instruct-name ('register-name expression))
;;
;; Helpers for make-assign


(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (assign-reg-name inst) (cadr inst))

(define (assign-value-exp inst) (cddr inst))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))


;; make-assign

(define (make-assign inst machine labels operations pc)
  (let ((target
	 (get-register machine assign-reg-name inst)))

    ;; construct value-proc
    (let ((value-proc
	   (if (operation-exp? value-exp)
	       (make-operation-exp
		value-exp machine labels operations)
	       (make-primitive-exp
		(car value-exp) machine labels))))
      (lambda ()
	(set-contents! target (value-proc))
	(advance-pc pc)))))


;; make-test
;;
;; TODO

(define (make-test ))
