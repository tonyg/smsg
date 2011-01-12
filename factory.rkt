#lang racket

(require "infra.rkt")
(require "directory.rkt")

(provide register-object-class!)

(define classes (make-hash))

(define (register-object-class! name factory-proc)
  (hash-set! classes name factory-proc))

(define factory
  (match-lambda
   [`(#"create" ,classname ,arg ,reply-sink ,reply-name)
    (cond
     ((hash-ref classes classname) =>
      (lambda (p)
	(let ((reply (p arg)))
	  (write `(create reply is ,reply)) (newline)
	  (when (positive? (bytes-length reply-sink))
	    (post! reply-sink reply-name (if (eq? reply #t)
					     `(#"create-ok")
					     `(#"create-failed" ,reply)))))))
     (else
      (report! `(factory class-name-not-found ,classname ,arg))))]))

(let ()
  (rebind-node! #"factory"
		#f
		factory)
  (void))
