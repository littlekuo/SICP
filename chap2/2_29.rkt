#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a
(define left-branch car)
(define (right-branch li) (car (cdr li)))

(define branch-length car)
(define (branch-structure br)
  (car (cdr br)))

;b
 (define (total-weight m) 
   (cond ((null? m) 0) 
         ((not (pair? m)) m) 
         (else (+ (total-weight (branch-structure (left-branch  m))) 
                  (total-weight (branch-structure (right-branch m))))))) 

;c
 (define (torque branch) 
   (* (branch-length branch) (total-weight (branch-structure branch)))) 

 (define (balanced? m) 
   (cond ((null? m) #t) 
         ((not (pair? m)) #t) 
         (else (and (= (torque (left-branch m)) (torque (right-branch m)))
                    (balanced? (branch-structure (left-branch m)))
                    (balanced? (branch-structure (right-branch m))))))) 

;d
(define (make-mobile1 left right) (cons left right))

(define (make-branch1 length structure)
  (cons length structure))

;need to change
(define left-branch1 car)
(define (right-branch1 mo) (cdr mo))

(define branch-length1 car)
(define (branch-structure1 br)
  (cdr br))

