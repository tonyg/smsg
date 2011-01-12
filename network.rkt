#lang racket

(require "infra.rkt")
(require "directory.rkt")
(require "factory.rkt")

(provide relay serve-on-port client (struct-out display-hint))

(struct display-hint (hint body))

(define local-container-name #"smsg")

(define (write-sexp x p)
  (cond
   ((bytes? x)
    (display (bytes-length x) p)
    (display #\: p)
    (display x p))
   ((pair? x)
    (display #\( p)
    (for-each (lambda (v) (write-sexp v p)) x)
    (display #\) p))
   ((display-hint? x)
    (display #\[ p)
    (write-sexp (display-hint-hint x) p)
    (display #\] p)
    (write-sexp (display-hint-body x) p))
   (else (error `(write-sexp bad-sexp ,x)))))

(define (read-simple-string buffer p)
  (let ((c (integer->char (read-byte p))))
    (cond
     ((eqv? c #\:) (read-bytes (string->number (list->string (reverse buffer))) p))
     ((char-numeric? c) (read-simple-string (cons c buffer) p))
     (else (error `(read-sexp syntax-error bad-simple-string-length ,c))))))

(define (read-sexp-list p)
  (let loop ((acc '()))
    (let ((v (read-sexp-inner p)))
      (if (eq? v 'end-of-list-marker)
	  (reverse acc)
	  (loop (cons v acc))))))

(define (read-sexp-inner p)
  (let ((b (read-byte p)))
    (if (eof-object? b)
	b
	(let ((c (integer->char b)))
	  (cond
	   ((eqv? c #\() (read-sexp-list p))
	   ((eqv? c #\)) 'end-of-list-marker)
	   ((eqv? c #\[)
	    (let ((hint (read-simple-string '() p)))
	      (when (not (eqv? (read-byte p) (char->integer #\])))
		(error `(read-sexp syntax-error display-hint)))
	      (display-hint hint (read-simple-string '() p))))
	   ((char-numeric? c) (read-simple-string (list c) p))
	   ((char-whitespace? c) (read-sexp p)) ;; convenience for testing
	   (else (error `(read-sexp syntax-error bad-character ,c))))))))

(define (read-sexp p)
  (let ((v (read-sexp-inner p)))
    (if (eq? v 'end-of-list-marker)
	(error `(read-sexp syntax-error unexpected-end-of-list))
	v)))

;; relay : input-port output-port (maybe symbol) (maybe symbol) -> node-fn
(define (relay in out localname servermode)
  (define (route message)
    ;;(write `(sending to client ,message)) (newline)
    (write-sexp message out)
    (newline out))

  (file-stream-buffer-mode out 'line)

  (if servermode
      (route `(#"hop" #"0"))
      (match (read-sexp in)
	[`(#"hop" #"0") (void)]))

  (route `(#"subscribe" ,localname #"" #"" #"" #""))

  (thread
   (lambda ()
     (command-loop in out route)))

  ;; TODO: wait here to learn name of remote peer
  (sleep 0.1)

  route)

(define (command-loop in out route)
  (let loop ()
    (let ((command (read-sexp in)))
      ;;(write `(received from client ,command))(newline)
      (when (handle-inbound-command command in out route)
	(loop)))))

(define (handle-inbound-command command in out route)
  (if (eof-object? command)
      (begin (close-output-port out)
	     (close-input-port in)
	     #f)
      (begin (match command
	       [`(#"subscribe" ,filter ,sink ,name ,reply-sink ,reply-name)
		(if (rebind-node! filter
				  #f
				  route)
		    (when (positive? (bytes-length reply-sink))
		      (post! reply-sink reply-name `(#"subscribe-ok" ,filter)))
		    (report! `(rebind-failed ,command)))]
	       [`(#"unsubscribe" ,id)
		(when (not (rebind-node! id
					 route
					 #f))
		  (report! `(rebind-failed ,command)))]
	       [`(#"post" ,name ,body ,token)
		(send! name body)]
	       [_ (report! `(illegal-command ,command))])
	     #t)))

(define (serve-on-port portnumber)
  (let ((server-sock (tcp-listen portnumber 5 #t)))
    (write `(listening on ,portnumber)) (newline)
    (let loop ()
      (let-values (((in out) (tcp-accept server-sock)))
	(write `(accepted-connection ,portnumber)) (newline)
	(relay in out local-container-name #t)
        (loop)))))

(define (client localname hostname portnumber)
  (let-values (((in out) (tcp-connect hostname portnumber)))
    (write `(connected ,hostname ,portnumber)) (newline)
    (relay in out localname #f)))

(register-object-class! #"client"
			(match-lambda
			 [`(,localname ,hostname ,portnumber)
			  (client localname hostname portnumber)]))
