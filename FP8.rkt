#lang scheme/base
(require racket/stream)

(define 3power
  (let 3power-gen ((res 1))
    (stream-cons res (3power-gen (* res 3)))
    )
  )

(define 5power
  (let 5power-gen ((res 1))
    (stream-cons res (5power-gen (* res 5)))
    )
  )

(define 3-5power
  (let interleave ((str3 3power) (str5 (stream-rest 5power))) ;чтобы была только одна 1
    (let ((next3 (stream-first str3)) (next5 (stream-first str5)))
      (if (< next3 next5) (stream-cons next3 (interleave (stream-restЫ str3) str5))
          (stream-cons next5 (interleave str3 (stream-rest str5)))
          )
      )
    )
  )