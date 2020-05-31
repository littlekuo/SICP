#lang racket


(define (deep-reverse li)
  (define (iter li cnt)
    (cond ((null? li) cnt)
          ((pair? (car li)) (iter (cdr li) (cons (deep-reverse (car li)) cnt)))
          (else (iter (cdr li) (cons (car li) cnt)))))
  (iter li empty))