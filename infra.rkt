#lang racket

(require "log.rkt")
(require "directory.rkt")

(provide post! unique-id)

(define (unique-id prefix)
  (string->symbol (string-append (symbol->string prefix)
				 "-"
				 (number->string (random #x10000) 16)
				 (number->string (random #x10000) 16)
				 (number->string (random #x10000) 16)
				 (number->string (random #x10000) 16))))

(define (post! sink name message [token #f])
  (write `(POSTING ,sink ,name ,message ,token))(newline)
  (when sink
    (lookup-node sink
		 (lambda (node)
		   (node `(post! ,name ,message ,token))
		   #t)
		 (lambda ()
		   (report! `(sink-not-found ,sink ,name ,message ,token))
		   #f))))
