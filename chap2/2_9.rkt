#lang racket

;  [aL, aH] + [bL, bH] = [aL + bL, aH + bH].
; width = width-a + width-b


; [aL, aH] - [bL, bH] = [aL - bH, aH - bL).
; width = width-a + width-b

(define (width-interval x)
  (/ ( - (upper-bound x) (lower-bound x)) 2))



