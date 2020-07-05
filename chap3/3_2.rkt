#lang racket

(define (make-monitored f)
  (define counter 0)
  (define (mf input)
    (cond ((eq? input 'how-many-calls?) counter)
          ((eq? input 'reset-count) (set! counter 0))
          (else (begin (set! counter (+ counter 1))
                       (f input)))))
  mf)