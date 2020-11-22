#lang scheme/base
(require scheme/mpair)

(define (make-queue) (mcons 'queue '()))

(define (queue? q) (and (mpair? q) (equal? 'queue (mcar q))))
(define (empty-queue? q) (and (queue? q) (null? (mcdr q))))

(define (front-queue q)
  (if (and (queue? q) (not (empty-queue? q)))
      (mcar (mreverse (mcdr q)))
      "empty queue")
  )

(define (insert-queue! q e)
  (if (queue? q)
      (set-mcdr! q (mcons e (mcdr q)))
      q)
  )
(define (delete-queue! q)
  (if (and (queue? q) (not (empty-queue? q)))
      (set-mcdr! q (mreverse (mcdr (mreverse (mcdr q)))))
      q)
  )

(define qq (make-queue))