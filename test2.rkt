#lang racket

(require "infra.rkt")
(require "directory.rkt")
(require "network.rkt")
(require "half-queue.rkt")

(define k (half-queue #"k"))

(client #"test2" "localhost" 5671)
(post! #"smsg" #"q1" `(#"post" #"" #"message1" #""))
(send! #"smsg" `(#"unsubscribe" #"test2"))
