#lang racket

(require "log.rkt")
(require "infra.rkt")
(require "directory.rkt")
(require "network.rkt")

(provide server)

(define (server portnumber)
  (let ((server-sock (tcp-listen portnumber 5 #t)))
    (let loop ()
      (let-values (((in out) (tcp-accept server-sock)))
	(write `(accepted-connection)) (newline)
	(relay in out #f #f)
        (loop)))))
