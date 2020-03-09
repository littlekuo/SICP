#lang racket


;prepare
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
(try first-guess))

(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(define (repeated f n)
  (if (= n 1) f
      (compose f (repeated f (- n 1)))))


; 

(define (exp base n)
  (if (< n 1)
      1
      (* base (exp base (- n 1)))))


(define (g x n) (lambda (y) (/ x (exp y (- n 1)))))

(define (log2 x) (/ (log x) (log 2))) 

(define (n-root x n)
  (fixed-point ((repeated average-damp (floor (log2 n))) (g x n)) 1.0))









