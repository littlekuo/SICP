#lang racket

(define (equal? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((or (null? x) (null? y)) #f)
        ((and (not (pair? x)) (not (pair? y))) (eq? x y))
        ((and (not (pair? x))) #f)
        ((and (not (pair? y))) #f)
        (else (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))))