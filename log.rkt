#lang racket

(provide report!)

(define (report! msg)
  (pretty-print msg)
  (void))
