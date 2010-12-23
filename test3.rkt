#lang racket

(require "infra.rkt")
(require "directory.rkt")
(require "client.rkt")
(require "half-queue.rkt")

(define k (half-queue 'k))

(client 'server 'test3 "localhost" 5671)
(post! 'server 'factory `(create! queue q1 test3 k))
(write (k)) (newline)

(do ((i 0 (+ i 1)))
    ((= i 1000000))
  (post! 'server 'q1 `(post! #f ,i #f)))
(lookup-node 'server (lambda (node) (node `(unsubscribe! test3))))
