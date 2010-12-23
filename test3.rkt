#lang racket

(require "infra.rkt")
(require "directory.rkt")
(require "network.rkt")
(require "half-queue.rkt")

(define k (half-queue 'k))

(client 'server 'test3 "localhost" 5671)
(post! 'server 'factory `(create! queue q1 test3 k))
(write (k)) (newline)

(do ((i 0 (+ i 1)))
    ((= i 1000000))
  (post! 'server 'q1 `(post! #f ,i #f)))
(send! 'server `(unsubscribe! test3))
