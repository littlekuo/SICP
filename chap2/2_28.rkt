#lang racket


(define (fringe tr)
  (cond ((null? tr) '())
        ((pair? (car tr)) (append (fringe (car tr)) (fringe (cdr tr))))
        (else (cons (car tr) (fringe (cdr tr))))))