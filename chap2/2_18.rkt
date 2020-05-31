#lang racket

(define nil '())

;it's not efficient
(define (reverse li)
  (if (null? li)
      li
      (append (reverse (cdr li)) (list (car li)))))


;
(define (reverse1 li) 
   (define (iter li result) 
     (if (null? li) 
         result 
         (iter (cdr li) (cons (car li) result)))) 
   (iter li nil)) 