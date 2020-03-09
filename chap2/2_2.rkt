#lang racket



(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)




(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment x)
  (make-point (average (car (start-segment x)) (car (end-segment x)))
              (average (cdr (start-segment x)) (cdr (end-segment x)))))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

