#lang racket

(require "infra.rkt")
(require "client.rkt")
(require "half-queue.rkt")

(define k (half-queue 'k))
(define c (half-queue 'consumer))

(client 'server 'test1 "localhost" 5671)
(post! 'server 'factory `(create! queue q1 test1 k))
(write (k)) (newline)
(post! 'server 'q1 `(subscribe! #f test1 consumer test1 k))
(write (k)) (newline)

(write `(first message is ,(c))) (newline)

(define start-time (current-inexact-milliseconds))
(define (elapsed) (/ (- (current-inexact-milliseconds) start-time) 1000.0))
(let loop ((i 2))
  (let ((m (c)))
    ;;(write `(received on consumer ,(c))) (newline)
    (when (zero? (modulo i 1000))
      (write `(after ,i messages we see ,(/ i (elapsed)) messages per second))
      (newline)))
  (loop (+ i 1)))
