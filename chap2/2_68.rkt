#lang racket

(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree)) (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree)
      (cadddr tree)))

;solutions
(define (element-of-set? x set) 
   (cond ((null? set) false) 
         ((equal? x (car set)) true) 
         (else (element-of-set? x (cdr set))))) 

   
(define (encode-symbol symbol tree) 
   (define (search symbol tree) 
     (cond ((leaf? tree) '()) 
           ((element-of-set? symbol (symbols (left-branch tree))) 
            (cons 0 (encode-symbol symbol (left-branch tree)))) 
           (else 
            (cons 1 (encode-symbol symbol (right-branch tree)))))) 
   (if (element-of-set? symbol (symbols tree)) 
       (search symbol tree)  
       (error "try to encode NO exist symbol" symbol)))      
        
        

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

