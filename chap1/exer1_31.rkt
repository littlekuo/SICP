#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; a
(define (factorial n)
  (define (f-term x) x)
  (define (f-next x) (+ x 1))
  (product f-term 1 f-next n))

(define (product-pi n)
  (define (p-term n)
    (if (even? n) (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (define (p-next n) (+ n 1))
  (product p-term 1 p-next n))

; b

(define (itera-product term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (* res (term a)))))
  (iter a 1))