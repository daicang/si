;; assembler.scm


(define (extract-labels text receive)
  (if (null? text)
      (receive)))

(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts! insts labels machine)
		    insts)))
