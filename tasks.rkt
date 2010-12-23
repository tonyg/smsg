#lang racket
(require data/queue)
(require srfi/1)

(struct task (mailbox
              [state #:mutable]
              [continuation #:mutable]))

(define idle-handler (lambda () (raise 'idle)))
(define current-task (task (make-queue)
                           'running
                           #f))
(define runnable-tasks (make-queue))

(define (start-task thunk)
  (let ((t (task (make-queue)
                 'running
                 (lambda (dummy)
                   (thunk)
                   (set-task-state! current-task 'dead)
                   (process-runnable-tasks!)))))
    (schedule-task! t)
    (yield!)
    t))

(define (schedule-task! task)
  (set-task-state! task 'running)
  (enqueue! runnable-tasks task))

(define (task-send task message)
  (when (eq? (task-state task) 'dead)
    (raise 'dead))
  (enqueue! (task-mailbox task) message)
  (when (eq? (task-state task) 'receiving)
    (schedule-task! task)))

(define (task-receive)
  (let loop ()
    (if (queue-empty? (task-mailbox current-task))
        (begin (set-task-state! current-task 'receiving)
               (suspend-and-switch!)
               (loop))
        (dequeue! (task-mailbox current-task)))))

(define (yield!)
  (schedule-task! current-task)
  (suspend-and-switch!))
  
(define (suspend-and-switch!)
  (call-with-current-continuation
   (lambda (cc)
     (set-task-continuation! current-task cc)
     (process-runnable-tasks!))))

(define (process-runnable-tasks!)
  (set! current-task #f)
  (if (queue-empty? runnable-tasks)
      ((idle-handler))
      (begin (set! current-task (dequeue! runnable-tasks))
             (let ((cc (task-continuation current-task)))
               (set-task-continuation! current-task #f)
               (cc (void))))))

(define x (map (lambda (i) (start-task (lambda ()
                                         (let ((m (task-receive)))
                                           (task-send (car m) (+ (cdr m) 1))))))
               (iota 10)))
(collect-garbage)