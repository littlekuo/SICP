#lang racket


(define (make-interval a b) (cons a b))

(define (upper-bound pair)
  (max (car pair) (cdr pair)))

(define (lower-bound pair)
  (min (car pair) (cdr pair)))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

