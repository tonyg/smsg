#lang racket

(require "log.rkt")
(require "infra.rkt")
(require "directory.rkt")
(require "factory.rkt")
(require "network.rkt")

(provide client)

(define (client localname remotename hostname portnumber)
  (let-values (((in out) (tcp-connect hostname portnumber)))
    (write `(connected ,hostname ,portnumber)) (newline)
    (relay in out localname remotename)))

(register-object-class! 'client
			(match-lambda
			 [`(,localname ,remotename ,hostname ,portnumber)
			  (client localname remotename hostname portnumber)]))
