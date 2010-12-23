#lang racket

(provide source-interface
         co-source-interface
         sink-interface)

(define source-interface
  (interface ()
    ;; name-filter target-sink target-name reply-sink reply-name -> void
    subscribe!
    ;; token -> void
    unsubscribe!))

(define sink-interface
  (interface ()
    ;; post! : name body (maybe token) -> void
    post!))

(define co-source-interface
  (interface ()
    ;; token -> void
    subscribe-ok!))
