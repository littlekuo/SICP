#lang sicp


;a
; number?, same-variable? are both predicates. there's nothing to dispatch. 


;b

;prepare
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(define (=number? x b) (and (number? x) (= x b)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))



(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;solution
(define (install-sum-package)
   (define (addend s) (car s))
    (define (augend s) (cadr s))
    (define (make-sum x y)
        (cond ((=number? x 0)
                y)
              ((=number? y 0)
                x)
              ((and (number? x) (number? y))
                (+ x y))
              (else
                (list '+ x y))))
    (put 'addend '+ addend)
    (put 'augend '+ augend)
    (put 'make-sum '+ make-sum)

    (put 'deriv '+
        (lambda (exp var)
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var))))
'done)

(define (make-sum x y)
    ((get 'make-sum '+) x y))





(define (install-product-package)
    (define (multiplier s)
        (car s))
    (define (multiplicand s)
        (cadr s))
    (define (make-product x y)
        (cond ((or (=number? x 0) (=number? y 0)) 0)
              ((=number? y 1) x)
              ((=number? x 1) y)
              ((and (number? x) (number? y))
                (* x y))
              (else
                (list '* x y))))
    (put 'multiplier '* multiplier)
    (put 'multiplicand '* multiplicand)
    (put 'make-product '* make-product)

    (put 'deriv '*
        (lambda (exp var)
            (make-sum (make-product (deriv (multiplier exp) var) (multiplicand exp))
                      (make-product (multiplier exp) (deriv (multiplicand exp) var)))))
'done)

(define (make-product x y)
    ((get 'make-product '*) x y))

;c
(define (install-exponent-package)
    (define (base expr)
      (car expr))
    (define (exponent expr)
      (cadr expr)) 
    (define (make-exponent base exponent) 
     (cond ((=number? exponent 0) 1) 
           ((=number? exponent 1) base) 
           ((=number? base 1) 1) 
           (else (list '** base exponent))))
  (define (exponent-deriv expr var) 
    (make-product (exponent expr) 
                  (make-product  
                    (make-exponent (base expr) 
                                         (make-sum (exponent expr) -1)) 
                    (deriv (base expr) var))))
  (put 'make-exponent '** make-exponent)
  (put 'deriv '** exponent-deriv)

  'done)




;test
(install-sum-package)
(install-product-package)
(install-exponent-package)
(deriv 0 'x)
(deriv '(+ x 0) 'x)
(deriv '(* x (* x x)) 'x)
(deriv '(** x 5) 'x)


;d
;The only thing  is changing the order of arguments in procedure "put". 

   
  
  
