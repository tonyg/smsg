#lang racket

(require "infra.rkt")
(require "directory.rkt")
(require "network.rkt")

(provide server)

(define (server portnumber)
  (let ((server-sock (tcp-listen portnumber 5 #t)))
    (write `(listening on ,portnumber)) (newline)
    (let loop ()
      (let-values (((in out) (tcp-accept server-sock)))
	(write `(accepted-connection ,portnumber)) (newline)
	(relay in out #f #f)
        (loop)))))
