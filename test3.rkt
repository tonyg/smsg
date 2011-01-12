#lang racket

(require "infra.rkt")
(require "directory.rkt")
(require "network.rkt")
(require "half-queue.rkt")

(define k (half-queue #"k"))

(client #"test3" "localhost" 5671)
(post! #"smsg" #"factory" `(#"create" #"queue" (#"q1") #"test3" #"k"))
(write (k)) (newline)

(do ((i 0 (+ i 1)))
    ((= i 1000000))
  (post! #"smsg" #"q1" `(#"post" #"" ,(string->bytes/utf-8 (number->string i)) #"")))
(send! #"smsg" `(#"unsubscribe" #"test3"))
