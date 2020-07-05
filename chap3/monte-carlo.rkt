#lang racket

(define (rand)
  

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remain trivals-passed)
    (cond ((= trials-remain 0)
           (/ trials-passed trials))
          ((expriment)
            (iter (- trials-remain 1)
                  (+ trials-passed 1)))
           (else
            (iter (- trials-remain 1)
                  trials-passed))))
  (iter trials 0))
                  