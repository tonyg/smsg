#lang racket

(require "log.rkt")
(require "infra.rkt")
(require "directory.rkt")

(provide register-object-class!)

(define classes (make-hash))

(define (register-object-class! name factory-proc)
  (hash-set! classes name factory-proc))

(define factory
  (match-lambda
   [`(create! ,classname ,arg ,reply-sink ,reply-name)
    (cond
     ((hash-ref classes classname) =>
      (lambda (p)
	(post! reply-sink reply-name `(create-complete! ,(p arg)))))
     (else
      (report! `(factory class-name-not-found ,classname ,arg))))]))

(let ()
  (rebind-node! 'factory
		#f
		factory)
  (void))
