#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (=number? exp num)
  (and (number? exp) (= exp num)))

;a
(define (make-sum a1 a2) 
   (cond ((=number? a1 0) a2) 
         ((=number? a2 0) a1) 
         ((and (number? a1) (number? a2)) (+ a1 a2))
         (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define addend car)
(define augend caddr)

(define (make-product a1 a2) 
   (cond ((or (=number? a1 0) (=number? a2 0)) 0)
         ((=number? a1 1) a2) 
         ((=number? a2 1) a1) 
         ((and (number? a1) (number? a2)) (* a1 a2))
         (else (list a1 '* a2))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define multiplier car)
(define multiplicand caddr)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp) (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var) (multiplicand exp))))
        (else(error "unknown expression type: DERIV" exp))))



;b

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;
(define *precedence-table* 
   '( (maxop . 10000) 
      (minop . -10000) 
      (+ . 0) 
      (* . 1) )) 
  

(define (sum1? expr) 
   (eq? '+ (smallest-op expr))) 

(define (product1? expr) 
   (eq? '* (smallest-op expr)))

 (define (smallest-op expr) 
   (accumulate (lambda (a b) 
                 (if (operator? a) 
                     (min-precedence a b) 
                     b)) 
               'maxop 
               expr))

(define (operator? x) 
   (define (loop op-pair) 
     (cond ((null? op-pair) #f) 
           ((eq? x (caar op-pair)) #t) 
           (else (loop (cdr op-pair))))) 
   (loop *precedence-table*)) 
  
(define (min-precedence a b) 
   (if (precedence<? a b) 
       a 
       b)) 
  
(define (precedence<? a b) 
   (< (precedence a) (precedence b))) 
  
(define (precedence op) 
   (define (loop op-pair) 
     (cond ((null? op-pair) 
            (error "Operator not defined -- PRECEDENCE:" op)) 
           ((eq? op (caar op-pair)) 
            (cdar op-pair)) 
           (else 
            (loop (cdr op-pair))))) 
   (loop *precedence-table*))

(define (augend1 expr) 
   (let ((a (cdr (memq '+ expr)))) 
     (if (singleton? a) 
         (car a) 
         a)))

(define (prefix sym list) 
   (if (or (null? list) (eq? sym (car list))) 
       '() 
       (cons (car list) (prefix sym (cdr list))))) 
  
(define (addend1 expr) 
   (let ((a (prefix '+ expr))) 
     (if (singleton? a) 
         (car a) 
         a)))

(define (make-sum1 a1 a2) 
   (cond ((=number? a1 0) a2) 
         ((=number? a2 0) a1) 
         ((and (number? a1) (number? a2)) 
          (+ a1 a2)) 
         (else (list a1 '+ a2))))


 (define (multiplier1 expr) 
   (let ((m (prefix '* expr))) 
     (if (singleton? m) 
         (car m) 
         m))) 
  
 (define (multiplicand1 expr) 
   (let ((m (cdr (memq '* expr)))) 
     (if (singleton? m) 
         (car m) 
         m))) 
  
(define (make-product1 m1 m2) 
   (cond ((=number? m1 1)  m2) 
         ((=number? m2 1)  m1) 
         ((or (=number? m1 0) (=number? m2 0))  0) 
         ((and (number? m1) (number? m2)) 
          (* m1 m2)) 
         (else (list m1 '* m2))))

(define (singleton? li)
  (= (length li) 1))

(define (deriv1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum1? exp) (make-sum1 (deriv1 (addend1 exp) var)
                              (deriv1 (augend1 exp) var)))
        ((product1? exp)
         (make-sum1
          (make-product1 (multiplier1 exp) (deriv1 (multiplicand1 exp) var))
          (make-product1 (deriv1 (multiplier1 exp) var) (multiplicand1 exp))))
        (else(error "unknown expression type: DERIV" exp))))
