#lang racket

(require "log.rkt")
(require "xqueue.rkt")
(require "infra.rkt")
(require "directory.rkt")
(require "factory.rkt")

(provide queue)

(struct subscription (id sink name))

(define (queue qname)
  (define mutex (make-semaphore 1))
  (define backlog (make-queue))
  (define waiters (make-queue))
  (define subscriptions (make-hash))

  (define (send-to-waiter! sub body)
    (post! (subscription-sink sub)
	   (subscription-name sub)
	   body
	   (subscription-id sub)))

  (define (wait-and-shovel)
    (semaphore-post mutex)
    (thread-receive)
    (shovel))

  (define (shovel)
    (semaphore-wait mutex)
    (write `(state ,qname ,(queue-empty? backlog) ,(queue-empty? waiters)))(newline)
    (if (queue-empty? backlog)
        (wait-and-shovel)
        (let ((body (dequeue! backlog))
              (examined (make-queue)))
          (let try-next ()
            (if (queue-empty? waiters)
                (begin (pushback! backlog body)
                       (set! waiters examined)
                       (wait-and-shovel))
                (let* ((token (dequeue! waiters))
                       (sub (unbox token)))
                  (if (or (not sub) (not (send-to-waiter! sub body)))
                      ;; TODO: decide what should happen if sink lookup fails. Keep sub?
                      (try-next)
                      (begin (queue-append! waiters examined)
                             (enqueue! waiters token)
                             (semaphore-post mutex)
                             (shovel)))))))))
    
  (define shovel-thread (thread shovel))

  (define interpreter
    (match-lambda
     [`(post! ,name ,body ,sender-token)
      (call-with-semaphore
       mutex
       (lambda ()
	 (enqueue! backlog body)
	 (thread-send shovel-thread 'throck)))]
     [`(subscribe! ,filter ,sink ,name ,reply-sink ,reply-name)
      (let* ((id (unique-id qname))
	     (token (box (subscription id sink name))))
	(call-with-semaphore
	 mutex
	 (lambda ()
	   (hash-set! subscriptions id token)
	   (enqueue! waiters token)
	   (thread-send shovel-thread 'throck)
	   (post! reply-sink reply-name `(subscribe-ok! ,id)))))]
     [`(unsubscribe! ,id)
      (call-with-semaphore
       mutex
       (lambda ()
	 (let ((token (hash-ref subscriptions id (lambda () #f))))
	   (write `(found ,id ,token))(newline)
	   (when token
	     (write `(unsubbing ,id ,token))(newline)
	     (hash-remove! subscriptions id)
	     (set-box! token #f)))))]))

  (rebind-node! qname #f interpreter))

(register-object-class! 'queue queue)
