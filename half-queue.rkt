#lang racket

(require racket/async-channel)

(require "log.rkt")
(require "xqueue.rkt")
(require "infra.rkt")
(require "directory.rkt")
(require "factory.rkt")

(provide half-queue)

(define (half-queue name)
  (define ch (make-async-channel))

  (rebind-node! name
		#f
		(lambda (message)
		  (async-channel-put ch message)))

  (lambda () (async-channel-get ch)))
