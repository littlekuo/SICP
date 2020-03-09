#lang racket



(define (cont-frac n d k) 
   (define (frac-rec i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (frac-rec (+ i 1))))))
  (frac-rec 1)) 

; a  11

(define (cont-frac1 n d k) 
   (define (loop i res)
     (if (> i k)
         res
         (loop (+ i 1) (/ (n i) (+ (d i) res)))))
  (loop 1 0))
         