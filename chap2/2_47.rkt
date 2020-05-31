#lang sicp

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (make-frame1 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame1 car)
(define (edge1-frame1 fr)
     (car (cdr fr)))
(define (edge2-frame1 fr)
  (cdr (cdr fr)))