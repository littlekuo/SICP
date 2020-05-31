#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t) 
   (accumulate + 0 (map (lambda (nod)
                          (cond ((null? nod) 0)
                                ((pair? nod) (count-leaves nod))
                                (else 1)))  t))) 