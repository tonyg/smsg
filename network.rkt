#lang racket

(require "log.rkt")
(require "infra.rkt")
(require "directory.rkt")
(require "factory.rkt")

(provide handle-inbound-command)

(define (handle-inbound-command command in out route)
  (if (eof-object? command)
      (begin (close-output-port out)
	     (close-input-port in)
	     #f)
      (begin (match command
	       [`(subscribe! ,filter ,sink ,name ,reply-sink ,reply-name)
		(if (rebind-node! filter
				  #f
				  route)
		    (post! reply-sink reply-name `(subscribe-ok! ,filter))
		    (report! `(rebind-failed ,command)))]
	       [`(unsubscribe! ,id)
		(when (not (rebind-node! id
					 route
					 #f))
		  (report! `(rebind-failed ,command)))]
	       [`(post! ,name ,body ,token)
		(lookup-node name (lambda (node) (node body)))]
	       [_ (report! `(illegal-command ,command))])
	     #t)))
