#lang racket


;prepare
(define nil '())

(define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 


(define (flatmap proc seq) 
   (accumulate append nil (map proc seq)))

(define (enumerate-interval low high) 
   (if (> low high) 
       nil 
       (cons low (enumerate-interval (+ low 1) high))))


;solution
(define empty-board '())

(define (adjoin-position new-row k queens)
  (cons (list new-row k) queens))

(define (safe? k positions)
  (define (is-safe pos)
    (let ((fst (car positions)))
      (cond ((= (car pos) (car fst)) #f)
            ((= (cadr pos) (cadr fst)) #f)
            ((= (- (cadr fst) (cadr pos)) (- (car fst) (car pos))) #f)
            ((= (- (cadr fst) (cadr pos)) (- (car pos) (car fst))) #f)
            (else #t))))
  (accumulate (lambda (x y) (and (is-safe x) y)) true (cdr positions)))



(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap (lambda (rest-of-queens)
                    (map (lambda (new-row)
                           (adjoin-position new-row k rest-of-queens))
                         (enumerate-interval 1 board-size)))
                  (queen-cols (- k 1))))))
  (queen-cols board-size))