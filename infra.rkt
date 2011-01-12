#lang racket

(require "directory.rkt")

(provide unique-id report! post! send!)

(define (random-chunk)
  (string->bytes/utf-8 (number->string (random #x10000) 16)))

(define (unique-id prefix)
  (bytes-append prefix
		#"-" (random-chunk)
		#"-" (random-chunk)
		#"-" (random-chunk)
		#"-" (random-chunk)))

(define (report! msg)
  (pretty-print msg)
  (void))

(define (post! sink name message [token #""])
  (send! sink `(#"post" ,name ,message ,token)))

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
