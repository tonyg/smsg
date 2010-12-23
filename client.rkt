#lang racket

(require "log.rkt")
(require "infra.rkt")
(require "directory.rkt")

(provide client)

(define (client arg)
  (match arg
    [`(,localname ,remotename ,hostname ,portnumber)
     (let-values (((in out) (tcp-connect hostname portnumber)))
       (define (route message)
	 (write `(relaying downstream ,message))(newline)
	 (write message out)
	 (newline out))

       (write `(connected ,hostname ,portnumber))(newline)
       (file-stream-buffer-mode out 'line)

       (route `(subscribe! ,remotename #f #f ,remotename login))
       (match (read in)
	 [`(post! login (subscribe-ok! ,_) ,_)

	  (define 
	  (if (rebind-node! localname
			    #f
			    
			    
	
  (let loop ()
    (let ((command (read in)))
      (write `(received ,command))(newline)
      (if (eof-object? command)
          (begin (close-output-port out)
                 (close-input-port in)
                 'done)
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
                 (loop))))))
