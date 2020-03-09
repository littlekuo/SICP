#lang racket


(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value term (next a) next b))))

;prime?
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (square x) (* x x))

(define (sum-square-prime a b)
  (define term square)
  (define (next x) (+ x 1))
  (filtered-accumulate prime? + 0 term a next b))

;gcd
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (product-relativeprime n)
  (define (term x) x)
  (define (next x) (+ x 1))
  (define (filter x) (= (gcd x n) 1))
  (filtered-accumulate filter * 1 term 1 next n))