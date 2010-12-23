#lang racket

(require "directory.rkt")

(provide unique-id report! post! send!)

(define (unique-id prefix)
  (string->symbol (string-append (symbol->string prefix)
				 "-"
				 (number->string (random #x10000) 16)
				 (number->string (random #x10000) 16)
				 (number->string (random #x10000) 16)
				 (number->string (random #x10000) 16))))

(define (report! msg)
  (pretty-print msg)
  (void))

(define (post! sink name message [token #f])
  (send! sink `(post! ,name ,message ,token)))

(define (send! sink body)
  ;; (write `(SENDING ,sink ,body)) (newline)
  (and sink
       (lookup-node sink
		    (lambda (node)
		      (node body)
		      #t)
		    (lambda ()
		      (report! `(sink-not-found ,sink ,body))
		      #f))))
