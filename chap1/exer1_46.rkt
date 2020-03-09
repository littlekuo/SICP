#lang racket


(define (iterative-improve good-enough? improve)
  (define (isok? x)
    (if (good-enough? x) x
        (isok? (improve x))))
  isok?)


; rewrite sqrt
(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (define (good-enough? y)
    (< (abs (- (square y) x)) 0.001))
  (define (improve y)
    (average y (/ x y)))
  ((iterative-improve good-enough? improve) guess))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;rewrite fixed-point
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v)
    (< (abs (- v (f v))) tolerance))
  ((iterative-improve close-enough? f) first-guess))

