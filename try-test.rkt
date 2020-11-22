#lang scheme/base
(require racket/list)

(define (taskI lst)
  (cddr (foldl (lambda (elem res)                          ; res - список вида (<номер последнего рассмотренного>
                          (let ((cur-min (cadr res)) (cur-i (+ (car res) 1))) ;            <значение минимума> <i1>...)
                            (cond ((> elem cur-min) (cons cur-i (cdr res)))
                                  ((= elem cur-min) (cons cur-i (cons cur-min (cons cur-i (cddr res))))) 
                                  (else (list cur-i elem cur-i))
                                  )
                            )
                          )
                        (list -1 (car lst))
                        lst
                        ))
  )

(define (taskII t s)
  (let inside ((tree t) (cur-mult 1))
    (if (vector? tree) (+ (inside (vector-ref tree 0) (* cur-mult 4)) (inside (vector-ref tree 1) (* cur-mult 4))
                          (inside (vector-ref tree 2) (* cur-mult 4)) (inside (vector-ref tree 3) (* cur-mult 4)))
        (* tree (/ s cur-mult))
     )
   )
  )

(define (taskIII lst)
  (let ((max-len (length (argmax length lst))))
    (map (lambda (elem) (if (= (length elem) max-len) (map add1 elem)
                            elem)
           )
         lst
         )
    )
  )