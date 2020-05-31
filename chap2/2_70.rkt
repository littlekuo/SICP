#lang racket


;prepare
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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

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


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pair-set)
  (cond ((null? pair-set) '())
        ((null? (cdr pair-set)) (car pair-set))
        (else (successive-merge (adjoin-set (make-code-tree (car pair-set) (cadr pair-set)) (cddr pair-set))))))

;solutions
(define the-huffman-tree
  (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))


(define the-result
  (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
            SHA BOOM) the-huffman-tree))
;(length the-result) is 84

; if we used a fixed-length code for the eight-symbol alphabet,
;then we used 108 bit. 