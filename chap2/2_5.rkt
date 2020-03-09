#lang racket

(define (my-cons a b) 
         (* (expt 2 a) (expt 3 b)))


(define (count-remainder-divisions n p d) 
         (if (= (remainder p d) 0) 
                 (count-remainder-divisions (+ n 1) (/ p d) d) 
                 n)) 

(define (my-car pair)
  (count-remainder-divisions 0 pair 2))

(define (my-cdr pair)
  (count-remainder-divisions 0 pair 3))
