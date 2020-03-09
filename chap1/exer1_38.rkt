#lang racket


(define nf (lambda (i) 1.0))

(define df
  (lambda (i)
    (cond ((= (remainder i 3) 1) 1)
          ((= (remainder i 3) 0) 1)
          (else (* 2 (+ (quotient i 3) 1))))))


(define (cont-frac n d k) 
   (define (loop i res)
     (if (> i k)
         res
         (loop (+ i 1) (/ (n i) (+ (d i) res)))))
  (loop 1 0))


;(+ 2 (cont-frac nf df k))
