#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        ("Insufficient money")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input action)
    (cond ((not (eq? input password)) (lambda (amount) "Incorrect password"))
          ((eq? action 'withdraw) withdraw)
          ((eq? action 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       action))))
   dispatch)