#lang racket


; recursive
(define  (f1 n)
  (cond ((< n 3) n)
        (else (+ (f1 (- n 1)) (* 2 (f1 (- n 2))) (* 3 (f1 (- n 3)))))))


; iterative
(define (f2 n)
  (cond ((< n 3) n)
        (else (itera 2 1 0 n))))

(define (itera a b c count)
  (if (= count 2)
       a
      (itera (+ a (* 2 b) (* 3 c)) a b (- count 1)))) 
       