#lang racket

(provide lookup-node
         rebind-node!)

(define mutex (make-semaphore 1))
(define table (make-hash))

(define (lookup-node name success-k [failure-k (lambda () #f)])
  (let ((result (call-with-semaphore
                 mutex
                 (lambda ()
                   (hash-ref table name (lambda () #f))))))
    (if result
        (success-k result)
        (failure-k))))

(define (rebind-node! name old new)
  (call-with-semaphore
   mutex
   (lambda ()
     (let ((actual-old (hash-ref table name (lambda () #f))))
       (if (eq? old actual-old)
           (begin (if new
                      (hash-set! table name new)
                      (hash-remove! table name))
                  #t)
           #f)))))
