#lang racket

(require "log.rkt")
(require "xqueue.rkt")
(require "infra.rkt")
(require "directory.rkt")
(require "factory.rkt")

(provide direct-exchange)

(struct subscription (id filter sink name))

(define (direct-exchange)
  (define mutex (make-semaphore 1))
  (define keys (make-hash))
  (define subscriptions (make-hash))

  (define (send-to-sub! sub body)
    (post! (subscription-sink sub)
	   (subscription-name sub)
	   body
	   (subscription-id sub)))

  (define interpreter
    (match-lambda
     [`(post! ,name ,body ,sender-token)
      (let ((subs (call-with-semaphore
		   mutex
		   (lambda ()
		     (hash-ref keys name (lambda () '()))))))
	(for-each (lambda (sub) (send-to-sub! sub body)) subs))]
     [`(subscribe! ,filter ,sink ,name ,reply-sink ,reply-name)
      (let* ((id (unique-id qname))
	     (sub (subscription id filter sink name)))
	(call-with-semaphore
	 mutex
	 (lambda ()
	   (hash-set! subscriptions id sub)
	   (hash-set! keys filter
		      (cons sub (hash-ref keys filter (lambda () '()))))
	   (post! reply-sink reply-name `(subscribe-ok! ,id)))))]
     [`(unsubscribe! ,id)
      (call-with-semaphore
       mutex
       (lambda ()
	 (let ((sub (hash-ref subscriptions id (lambda () #f))))
	   (when sub
	     (let ((subs (remove sub (hash-ref keys (subscription-filter sub)))))
	       (if (null? subs)
		   (hash-remove! keys (subscription-filter sub))
		   (hash-set! keys (subscription-filter sub) subs)))
	     (hash-remove! subscriptions id)))))]))

  interpreter)

(register-object-class! 'direct-exchange
			(lambda (arg)
			  (rebind-node! (first arg) #f (direct-exchange))))
