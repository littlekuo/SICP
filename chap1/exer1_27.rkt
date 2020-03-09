#lang racket

 (define (square x) (* x x)) 
  
 (define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (remainder (square (expmod base (/ exp 2) m)) 
                     m)) 
         (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m))))

(define (full-fermat-test? n)
  (define (iter a n)
    (if (= a n) true
        (if (= (expmod a n n) a) (iter (+ a 1) n)
            false)))
  (iter 1 n))



