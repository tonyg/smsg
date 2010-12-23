#lang racket

(require "infra.rkt")
(require "directory.rkt")
(require "factory.rkt")

(provide relay serve-on-port client)

;; relay : input-port output-port (maybe symbol) (maybe symbol) -> node-fn
(define (relay in out localname remotename)
  (define (route message)
    (write message out)
    (newline out))

  (file-stream-buffer-mode out 'line)

  (when remotename
    (route `(subscribe! ,remotename #f #f ,remotename login))
    (match (read in)
      [`(post! login (subscribe-ok! ,_) ,_)
       (void)]))

  (thread
   (lambda ()
     (command-loop in out route)))

  (when localname
    (rebind-node! localname #f route))

  route)

(define (command-loop in out route)
  (let loop ()
    (let ((command (read in)))
      ;;(write `(received from client ,command))(newline)
      (when (handle-inbound-command command in out route)
	(loop)))))

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
		(send! name body)]
	       [_ (report! `(illegal-command ,command))])
	     #t)))

(define (serve-on-port portnumber)
  (let ((server-sock (tcp-listen portnumber 5 #t)))
    (write `(listening on ,portnumber)) (newline)
    (let loop ()
      (let-values (((in out) (tcp-accept server-sock)))
	(write `(accepted-connection ,portnumber)) (newline)
	(relay in out #f #f)
        (loop)))))

(define (client localname remotename hostname portnumber)
  (let-values (((in out) (tcp-connect hostname portnumber)))
    (write `(connected ,hostname ,portnumber)) (newline)
    (relay in out localname remotename)))

(register-object-class! 'client
			(match-lambda
			 [`(,localname ,remotename ,hostname ,portnumber)
			  (client localname remotename hostname portnumber)]))
