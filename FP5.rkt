#lang scheme/base
(require racket/vector)

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (tree-empty? t) (equal? t #()))

(define (print-tree-by-level-desc tree)
  (map (lambda (x) (if x (printf "~a " x)
                       (printf "\n")))
       (cdr (let level-search ((queue (list tree #f)) (res '()))
         (let ((next-elem (car queue)))
           (if (equal? next-elem #f)
               (if (null? (cdr queue)) res
                   (level-search (append (cdr queue) '(#f)) (cons #f res))
                   )
               (if (tree-empty? next-elem) (level-search (cdr queue) res)
                   (level-search (append (cdr queue) (list (tree-left next-elem) (tree-right next-elem)))
                                 (cons (tree-data next-elem) res)) 
                   )
               )
           )
         ))
       )
  )

(define (print-tree-by-level-desc tree)
  (let level-search
   )
  )