#lang racket

(define (square x) (* x x))

;It is possible to prove that if such a nontrivial square root of 1 exists, then n is not prime

;Miller-Rabin test

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-nontrivial-sqrt (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;procedure returns 0 if n is a non-trival square root of 1 modulo m, otherwise it returns the square of n modulo m
(define (check-nontrivial-sqrt n m)
  (let ((x (remainder (square n) m)))
    (if (and (not (= n 1)) (not (= n (- m 1))) (= x 1))
        0
        x)))

(define (miller-rabin-test n a)
  (= (expmod a (- n 1) n) 1))

;
(define (prime? n)
  (cond ((= n 2) #t)
        ((even? n) #f)
        (else
         (prime-helper n (- n 1)))))
 
(define (prime-helper n a)
  (cond ((= a 0) #t)
        ((miller-rabin-test n a) (miller-rabin-test n (- a 1)))
        (else #f)))

