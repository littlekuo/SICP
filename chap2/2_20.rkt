#lang racket

(define (is-same-parity x y)
  (cond ((and (odd? x) (odd? y)) #t)
        ((and (even? x) (even? y)) #t)
        (else #f)))

; recursive
(define (same-parity x . y)
  (define (same-par fst rest)
    (cond ((null? rest) (list fst))
          ((is-same-parity fst (car rest)) (cons fst (same-par (car rest) (cdr rest))))
          (else (same-par fst (cdr rest)))))
  (same-par x y))

; iterative
(define (same-parity1 n . lst) 
    (define same-parity? (if (even? n) even? odd?)) 
    (define (iter lst acc) 
      (if (null? lst) 
          acc 
          (let ((first (car lst)) 
                (rest (cdr lst))) 
            (iter rest 
                  (if (same-parity? first) 
                      (cons first acc) 
                      acc))))) 
    (cons n (reverse (iter lst null)))) 

  