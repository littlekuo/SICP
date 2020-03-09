#lang racket

(define (runtime) (current-milliseconds))

(define (square x) (* x x)) 

; count the time
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


; 
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


; my
(define (search-for-primes n count) 
   (if (even? n) 
       (s-f-p (+ n 1) count) 
       (s-f-p n count)))

(define (s-f-p n count) 
     (if (> count 0) 
       (if (timed-prime-test n) 
           (s-f-p (+ n 2) (- count 1)) 
           (s-f-p (+ n 2) count)) 
       "COMPUTATION COMPLETE")) 