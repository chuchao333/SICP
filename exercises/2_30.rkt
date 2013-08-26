#lang racket


(define nil (list))

(define (square-tree t)
  (cond ((null? t) nil)
        ((not (pair? t)) (sqr t))
        (else (cons (square-tree (car t))
                    (square-tree (cdr t))))))


; treat the tree as a sequence of sub-trees and map over it
(define (square-tree-1 t)
  (define (map proc l)
    (if (null? l)
      nil
      (cons (proc (car l))
            (map proc (cdr l)))))
  (map (lambda (subtree)
          (if (pair? subtree)
            (square-tree-1 subtree)
            (sqr subtree)))
       t))

(define t (cons (list 1 2) (list 3 4)))

(square-tree t)
(square-tree-1 t)
