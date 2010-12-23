#lang racket

(require "infra.rkt")
(require "directory.rkt")
(require "client.rkt")
(require "half-queue.rkt")

(define k (half-queue 'k))

(client (list 'server 'test2 "localhost" 5671))
(post! 'server 'q1 `(post! #f message1 #f))
(lookup-node 'server (lambda (node) (node `(unsubscribe! test2))))
