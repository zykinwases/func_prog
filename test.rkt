#lang scheme/base
(require math/number-theory)

;1
(define (fun1 lst)
  (if (null? lst) '() ;res - список (<max> <pred-max>)
      (cadr (foldl (lambda (n res) (let ((loc-max (car res)) (pred-max (cadr res)))
                               (cond ((> n loc-max) (list n loc-max))
                                     ((= n loc-max) res)
                                     ((or (null? pred-max) (> n pred-max)) (list loc-max n))
                                     (else res))))
             (list (car lst) '())
             lst
       ))
   )
  )

;2
(define (is-not-prime? n)
  (if (< n 4) #f
      (ormap (lambda (x) (integer? (/ n x))) (cddr (build-list n values)))
      )
  )

(define (fun2a n)
  (let loop ((cur-i 4))
    (if (= cur-i n) (list n)
        (if (and (is-not-prime? cur-i) (integer? (/ n cur-i))) (cons cur-i (loop (add1 cur-i)))
            (loop (add1 cur-i)))
        )
    )
  )

(define (fun2b n)
  (let loop ((cur-i 4) (res '()))
    (if (> cur-i n) (reverse res)
        (loop (add1 cur-i) (if (and (is-not-prime? cur-i) (integer? (/ n cur-i))) (cons cur-i res) res))
        )
    )
  )

;3
(define (fun3 n)
  (let loop ((n n) (cur-mult 4) (res 1))
    (cond ((= n 0) res)
          ((prime? cur-mult) (loop n (add1 cur-mult) res))
          (else (loop (sub1 n) (add1 cur-mult) (* res cur-mult)))
     )
    )
  )

;4
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (is-empty? tree) (equal? tree #()))

(define (fun4 tree r1 r2)
  (let loop ((t tree) (from (min r1 r2)) (to (max r1 r2)))
    (cond ((is-empty? t) 0)
          ((= to 0) (if (positive? (tree-data t)) 1 0))
          ((< from 0) (+ (if (positive? (tree-data t)) 1 0)
                         (loop (tree-left t) (sub1 from) (sub1 to))
                         (loop (tree-right t) (sub1 from) (sub1 to))))
          (else (+ (loop (tree-left t) (sub1 from) (sub1 to))
                   (loop (tree-right t) (sub1 from) (sub1 to))))
          )
    )
  )

;5
(define (fun5-cps tree r1 r2 cc)
  (let loop-cps ((t tree) (from (min r1 r2)) (to (max r1 r2)) (cc cc))
    (cond ((is-empty? t) (cc 0))
          ((= to 0) (cc (if (positive? (tree-data t)) 1 0)))
          ((< from 0) (loop-cps (tree-left t) (sub1 from) (sub1 to)
                                (lambda (y) (loop-cps (tree-right t) (sub1 from) (sub1 to)
                                                      (lambda (z) (cc (+ (if (positive? (tree-data t)) 1 0)
                                                                         y
                                                                         z)))))))
          (else (loop-cps (tree-left t) (sub1 from) (sub1 to)
                                (lambda (y) (loop-cps (tree-right t) (sub1 from) (sub1 to)
                                                      (lambda (z) (cc (+ y z)))))))
          )
    )
  )