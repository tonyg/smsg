#lang racket

(require "log.rkt")
(require "infra.rkt")
(require "directory.rkt")
(require "factory.rkt")

(provide relay)

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
		(lookup-node name (lambda (node) (node body)))]
	       [_ (report! `(illegal-command ,command))])
	     #t)))
