#lang racket

(define (square x) (* x x))

(define (square-tree tr)
  (cond ((null? tr) '())
        ((not (pair? tr)) (square tr))
        (else (cons (square-tree (car tr)) (square-tree (cdr tr))))))

(define (square-tree1 tr)
  (map (lambda (subtree)
         (if (pair? subtree) (square-tree1 subtree)
             (square subtree))) tr))