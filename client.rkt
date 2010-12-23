#lang racket

(require "log.rkt")
(require "infra.rkt")
(require "directory.rkt")
(require "factory.rkt")
(require "network.rkt")

(provide client)

(define (client arg)
  (match arg
    [`(,localname ,remotename ,hostname ,portnumber)
     (let-values (((in out) (tcp-connect hostname portnumber)))
       (define (route message)
	 ;;(write `(relaying downstream ,message))(newline)
	 (write message out)
	 (newline out))

       (write `(connected ,hostname ,portnumber))(newline)
       (file-stream-buffer-mode out 'line)

       (route `(subscribe! ,remotename #f #f ,remotename login))
       (match (read in)
	 [`(post! login (subscribe-ok! ,_) ,_)

	  (define handler-thread
	    (thread
	     (lambda ()
	       (let loop ()
		 (let ((command (read in)))
		   ;;(write `(received from server ,command))(newline)
		   (when (handle-inbound-command command in out route)
		     (loop)))))))

	  (rebind-node! localname
			#f
			route)]))]))

(register-object-class! 'client client)
