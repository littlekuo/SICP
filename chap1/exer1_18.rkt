#lang racket

(define (fast-mult a b)
  (mult-iter a b 0))

(define (mult-iter a b count)
  (cond ((= b 0) count)
        ((even? b) (mult-iter (double a) (halve b) count))
        (else (mult-iter (double a) (halve (- b 1)) (+ count a)))))


(define (double a)
  (* 2 a))

(define (halve b)
  (/ b 2))