#lang scheme/base

;1a
(define (iter-list-fib-squares n)
  (map (lambda (x) (* x x))
       (let fib-list ((i n) (fib-n-1 1) (fib-n-2 1) (result '()))
         (if (= i 0) (reverse result)
             (fib-list (- i 1) (+ fib-n-1 fib-n-2) fib-n-1 (cons fib-n-2 result))
             )
         )
       )
  )

;1b
(define (fold-list-fib-squares n)
  (reverse (foldl (lambda (x y) (cons (* x x) y))
                  '()
                  (let fib-list ((i n) (fib-n-1 1) (fib-n-2 1) (result '()))
                    (if (= i 0) (reverse result)
                        (fib-list (- i 1) (+ fib-n-1 fib-n-2) fib-n-1 (cons fib-n-2 result))
                        )
                    )
                  )
           )
  )


;2
(define (process lst)
  (let ((comp-value (foldl * 1 (car lst))))
    (filter (lambda (x) (< comp-value (foldl + 0 x))) (cdr lst))
    )
  )