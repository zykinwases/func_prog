#lang scheme/base
(define (coplanar? x0 y0 z0 x1 y1 z1 x2 y2 z2)
  (let ((triple_product (+ (* x0 y1 z2) (* x1 y2 z0) (* y0 z1 x2) (-(* x2 y1 z0)) (-(* y0 x1 z2)) (-(* x0 y2 z1)))))
    (if (= triple_product 0) #t
        #f
     )
    )
  )