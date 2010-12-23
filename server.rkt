#lang racket

(require "log.rkt")
(require "infra.rkt")
(require "directory.rkt")
(require "network.rkt")

(provide server)

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
    ;;(write `(relaying upstream ,message))(newline)
    (write message out)
    (newline out))

  (write `(accepted-connection))(newline)
  (file-stream-buffer-mode out 'line)
  (let loop ()
    (let ((command (read in)))
      ;;(write `(received from client ,command))(newline)
      (when (handle-inbound-command command in out route)
	(loop)))))
