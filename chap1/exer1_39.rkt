#lang racket


(define (cont-frac n d k) 
   (define (frac-rec i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (frac-rec (+ i 1))))))
  (frac-rec 1))


(define (tan-cf x k)
  (define n-tan
    (lambda (i)
      (if (= i 1) x
          (* x -1 x))))
  (define d-tan (lambda (i) (- (* 2 i) 1)))
  (cont-frac n-tan d-tan k))


