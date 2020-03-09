#lang racket

(define (make-interval a b) (cons a b))

(define (upper-bound pair)
  (max (car pair) (cdr pair)))

(define (lower-bound pair)
  (min (car pair) (cdr pair)))


;classify (3*3 = 9)

(define (mul-interval x y)
  (define (positive? x) (>= x 0))
  (define (negative? x) (< x 0))
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y))) 
     (cond ((and (positive? xl) (positive? yl)) 
            (make-interval (* xl yl) (* xu yu))) 
           ((and (positive? xl) (negative? yl)) 
            (make-interval (* xu yl) (* (if (negative? yu) xl xu) yu))) 
           ((and (negative? xl) (positive? yl)) 
            (make-interval (* xl yu) (* (if (negative? xu) yl yu) xu)))
           ((and (negative? xu) (negative? yl))
            (if (negative? yu) (make-interval (* xu yu) (* xl yl))
                (make-interval (* xl yu) (* xl yl))))
           ((negative? yu) (make-interval (* xu yl) (* xl yl)))
           (else
            (let ((l (min (* xl yu) (* xu yl))) 
                  (u (max (* xl yl) (* xu yu)))) 
              (make-interval l u))))))



            
    
  