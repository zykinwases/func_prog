#lang scheme/base

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (tree-empty? t) (equal? t #()))

(define (task-4-2020 h)
  (let fact-tree ((i 0) (cur-fact 1))
    (if (= i h) empty-tree
        (let* ((next-i (+ i 1)) (branch (fact-tree next-i (* cur-fact next-i))))
          (make-tree cur-fact branch branch)
          )
        )
    )
  )