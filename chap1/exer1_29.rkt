#lang racket

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
       0
       (+ (term a)
          (sum term (next a) next b))))

(define (simpon-cube f a b n)
  (define (get-value k)
         (let ((h (/ (- b a) n))) (/ (* h (f (+ a (* k h)))) 3)))
  (define (term k)
      (cond ((or (= k 0) (= k n)) (get-value k))
            ((even? k) (* 2 (get-value k)))
            (else (* 4 (get-value k)))))
  (define (next x) (+ x 1))
  (sum term 0 next n))
  
    
      
      