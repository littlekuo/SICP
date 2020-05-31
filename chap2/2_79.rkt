#lang racket


(require "tag.rkt")
(require "apply-generic.rkt")
(require "put-and-get.rkt")

(define (equ? x y)
  (apply-generic 'equ? x y)) 


(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  ;add
  (put 'equ? '(scheme-number scheme-number)
     (lambda (x y)
       (= x y)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
'done)

(define (make-scheme-number n)
((get 'make 'scheme-number) n))


(define (install-rational-package)
;; internal procedures
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (make-rat n d)
(let ((g (gcd n d)))
(cons (/ n g) (/ d g))))
(define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ;add
  (put 'equ? '(rational rational)
        (lambda (x y)
            (and (= (numer x) (numer y))
                 (= (denom x) (denom y)))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


(require "polar-package.rkt")
(require "rectangular-package.rkt")

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))



(define (install-complex-package)
;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (tag z) (attach-tag 'complex z))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
   ;add
   (put 'equ? '(complex complex)
        (lambda (x y)
            (and (= (real-part x) (real-part y))
                 (= (imag-part x) (imag-part y)))))
'done)

(define (make-complex-from-real-imag x y)
((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
((get 'make-from-mag-ang 'complex) r a))
  