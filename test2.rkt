#lang racket

(require "infra.rkt")
(require "directory.rkt")
(require "network.rkt")
(require "half-queue.rkt")

(define k (half-queue 'k))

(client 'server 'test2 "localhost" 5671)
(post! 'server 'q1 `(post! #f message1 #f))
(send! 'server `(unsubscribe! test2))
