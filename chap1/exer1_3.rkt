#lang racket

(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

(define largeSum
  (lambda (x y z)
    (if (< x y)
        (if (< x z)
            (sum-of-squares y z)
            (sum-of-squares x y))
        (if (< y z)
            (sum-of-squares x z)
            (sum-of-squares x y)))))
           
          
                   
