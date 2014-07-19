(define-module debug.message-queue
  (use gauche.threads)
  (use util.queue)
  (export make-message-queue
    post post/result-wait
    get/wait set-result
    ))

(select-module debug.message-queue)

(define get-mtqueue (cut vector-ref <> 0))
(define get-result-box (cut vector-ref <> 1))
(define get-result-wait-mutex (cut vector-ref <> 2))
(define get-result-wait-condvar (cut vector-ref <> 3))

(define (make-message-queue)
  (vector
    (make-mtqueue)
    (cons #f #f) ;;result box
    (make-mutex) ;;result wait mutex
    (make-condition-variable) ;;result wait condvar
    ))

(define (post queue msg)
  (enqueue! (get-mtqueue queue) msg))

(define (post/result-wait queue msg)
  (post queue msg)
  (let ([mutex (get-result-wait-mutex queue)]
        [condvar (get-result-wait-condvar queue)]
        [result-box (get-result-box queue)])
    (mutex-unlock! mutex condvar)
    (mutex-lock! mutex)
    (car result-box)))

(define (get/wait queue)
  (dequeue/wait! (get-mtqueue queue)))

(define (set-result queue result)
  (let ([mutex (get-result-wait-mutex queue)]
        [condvar (get-result-wait-condvar queue)]
        [result-box (get-result-box queue)])
    (mutex-lock! mutex)
    (set-car! result-box result)
    (condition-variable-signal! condvar)
    (mutex-unlock! mutex)))

