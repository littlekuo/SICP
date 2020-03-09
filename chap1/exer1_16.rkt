#lang racket

(define (fast-expt b n)
  (faste 1 b n))

(define (faste a b n)
  (cond ((= n 0) a)
        ((= (remainder n 2) 0) (faste a (* b b) (quotient n 2)))
        (else (faste (* a b) (* b b) (quotient n 2)))))