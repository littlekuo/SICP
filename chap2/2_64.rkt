#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
(list entry left right))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree) remaining-elts))))))))
;a.
;PARTIAL-TREE splits the list ELTS into three parts:
;the median item THIS-ENTRY, the list of items less than the median, and the list of items greater than the median.
;It creates a binary tree whose root node is THIS-ENTRY,
;whose left subtree is the PARTIAL-TREE of the smaller elements, and whose right subtree is the PARTIAL-TREE of the larger elements.


;b.
;T(n) = 2T(n/2) + Θ(1)
;T(n) = Θ(n)
