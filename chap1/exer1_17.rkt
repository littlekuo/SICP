#lang racket


; recursion
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ (fast-mult (double a) (halve (- b 1))) a))))

(define (double n)
  (* 2 n))

(define (halve n)
  (/ n 2))