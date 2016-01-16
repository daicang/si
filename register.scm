;; register.scm

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch msg)
      (cond ((eq? msg 'get) contents)
	    ((eq? msg 'set)
	     (lambda (value)
	       (set! contents value)))
	    (else
	     (error "register: Unknown request" msg))))
    dispatch))

(define (get-contents reg)
  (reg 'get))

(define (set-contents! reg)
  (reg 'set))
