#lang racket

(define (make-interval a b) (cons a b))

(define (upper-bound pair)
  (max (car pair) (cdr pair)))

(define (lower-bound pair)
  (min (car pair) (cdr pair)))

