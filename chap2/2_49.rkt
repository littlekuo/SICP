#lang racket


;draw-line
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (vector-to-posn v)
  (make-posn (car v) (cdr v)))

;prepare
(define make-vect cons)

(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (cons (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (cons (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (cons (* s (xcor-vect v)) (* s (ycor-vect v))))


(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (make-segment x1 y1 x2 y2)
  (cons (make-vect x1 y1) (make-vect x2 y2)))

(define start-segment car)

(define end-segment cdr)


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (line
        (vector-to-posn ((frame-coord-map frame)
         (start-segment segment)))
        (vector-to-posn ((frame-coord-map frame)
         (end-segment segment)))))
     segment-list)))


; a
(define (painter-outline frame)
  (let ((segment-list (list (make-segment 0 0 0 1) (make-segment 0 0 1 0) (make-segment 0 1 1 1) (make-segment 1 0 1 1))))
    ((segments->painter segment-list) frame)))
; b. 
(define (painter-x frame)
  (let ((segment-list (list (make-segment 0 1 1 0) (make-segment 0 0 1 1))))
    ((segments->painter segment-list) frame)))
; c.
(define (painter-diamond frame)
  (let ((segment-list (list (make-segment 0 0.5 0.5 0) (make-segment 0 0.5 0.5 1) (make-segment 0.5 0.0 1.0 0.5) (make-segment 1 0.5 0.5 1))))
    ((segments->painter segment-list) frame)))

; d


(define f0 (make-frame (make-vect 30 40) (make-vect 60 10) (make-vect 0 60)))
