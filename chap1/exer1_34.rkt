#lang racket

;
(define (f g)
  (g 2))

;(f f) -> (f 2) -> (2 2)
; but 2 is not a procedure, so it will causes error.