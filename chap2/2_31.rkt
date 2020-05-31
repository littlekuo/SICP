#lang racket

(define (tree-map g tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (g tree))
        (else (cons (tree-map g (car tree)) (tree-map g (cdr tree))))))


(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))