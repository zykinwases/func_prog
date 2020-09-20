#lang scheme/base

(define (task-03-2020 lst)
  (sqrt (
         foldl (lambda (x y) (+ (* x x) y))
               0
               (map (lambda (sublist) (/ (foldl + 0 sublist) (length sublist)))
                      lst
                    )
         )
   )
  )