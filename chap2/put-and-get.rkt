#lang racket

(provide (all-defined-out))

(define *table* (make-hash))
(define (get op type) (hash-ref *table* (list op type) false))
(define (put op type val) (hash-set! *table* (list op type) val))