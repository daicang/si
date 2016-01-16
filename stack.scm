;; stack.scm
;;
;; Use:
;; (defiane s (make-stack))
;; (s 'initialize)
;; (push s 1)
;; (pop s)


(define (make-stack)
  (let ((s '()))
    
    (define (push x)
      (set! s (cons x s)))
    
    (define (pop)
      (if (null? s)
	  (error "pop: Empty stack")
	  (let ((top (car s)))
	    (set! s (cdr s))
	    top)))
    
    (define (initialize)
      (set! s '())
      'done)
    
    (define (dispatch msg)
      (cond ((eq? msg 'push) push)
	    ((eq? msg 'pop) (pop)) ;; return value instead of function
	    ((eq? msg 'initialize) (initialize))
	    (else
	     (error "make-stack: Unknown request: " message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;; (define (save reg)
;;   )

;; (define (restore reg))
