#lang scheme/base
(define (iter-2n-1!-list n)
  (if (< n 1) '()
      (let loop ((i 2) (acc 1) (result '(1)))
        (if (> i n) (reverse result)
            (let* ((next-step (* (- (* 2 i) 1) (- (* 2 i) 2) acc)) (res (cons next-step result)))
              (loop (+ i 1) next-step res)))
        )
      )
  )

(define (recur-2n-1!-list n)
  (cond ((< n 1) '())
        ((= n 1) '(1))
        (else (let ((my-list (reverse (recur-2n-1!-list (- n 1)))))
                (reverse (cons (* (- (* 2 n) 1) (- (* 2 n) 2) (car my-list)) my-list)
                         )
                )
              )
        )
  )