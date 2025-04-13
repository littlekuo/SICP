#lang racket

;pascal i j means the element in (i,j)
(define (pascal i j)
  (cond ((or (= j 0) (= j i)) 1)
        (else (+ (pascal (- i 1) (- j 1)) (pascal (- i 1) j)))))       