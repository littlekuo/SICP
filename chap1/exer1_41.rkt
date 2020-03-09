#lang racket

(define (double pro)
  (lambda (x)
    (pro (pro x))))

(define inc (lambda (x) (+ x 1)))