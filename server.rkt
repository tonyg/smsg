#lang racket

(require "log.rkt")
(require "infra.rkt")
(require "directory.rkt")

(provide server)

(struct mapping (sink name))

(define (server portnumber)
  (let ((server-sock (tcp-listen portnumber 5 #t)))
    (let loop ()
      (let-values (((in out) (tcp-accept server-sock)))
        (thread
         (lambda ()
           (serve in out)))
        (loop)))))

(define (serve in out)
  (define (route message)
    (write `(relaying upstream ,message))(newline)
    (write message out)
    (newline out))

  (write `(accepted-connection))(newline)
  (file-stream-buffer-mode out 'line)
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
