#lang racket

(define nil empty)

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))


(define (map proc items)
  (if (null? items) nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (square-list1 items)
  (map (lambda (x) (* x x)) items))