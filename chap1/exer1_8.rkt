#lang racket

(define (cube-root x)
  (cuber 1.0 x))

(define (cuber guess x)
  (if (good-enough? guess x)
      guess
      (cuber (improve guess x) x))) 

(define (improve y x)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (good-enough? guess x)
  (= (improve guess x) guess))

 