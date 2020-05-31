#lang racket


(define (last-pair li)
  (if (null? (cdr li))
       li
      (last-pair (cdr li)))) 