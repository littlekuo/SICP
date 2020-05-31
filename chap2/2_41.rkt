#lang racket

(define nil empty)

(define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 
  
(define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
(define (enumerate-interval low high) 
   (if (> low high) 
       nil 
       (cons low (enumerate-interval (+ low 1) high)))) 
  
(define (flatmap proc seq) 
   (accumulate append nil (map proc seq)))

(define (permutations s)
  (if (null? s); empty set?
      (list nil) 
      (flatmap
       (lambda (x) (map (lambda (p) (cons x p))
                        (permutations (remove x s))))
       s)))


;solution
(define (unique-tuples n k)
  (cond ((< n k) nil)
        ((= k 0) (list nil))
        (else (append (unique-tuples (- n 1) k) 
                         (map (lambda (tuple) (cons n tuple)) 
                              (unique-tuples (- n 1) (- k 1))))))) 


(define (unique-tuples1 n k) 
     (define (iter m k) 
         (if (= k 0) 
             (list nil) 
             (flatmap (lambda (j) 
                         (map (lambda (tuple) (cons j tuple)) 
                             (iter (+ j 1) (- k 1)))) 
                     (enumerate-interval m n)))) 
     (iter 1 k)) 


(define (find-triples n s)
  (define (is-sum-s tr)
    (= (accumulate + 0 tr) s)) 
  (filter is-sum-s (unique-tuples n 3)))



                     