#lang racket


(define nil (list))

(define (tree-map proc tree)
  (map (lambda (subtree)
         (if (pair? subtree)
           (tree-map proc subtree)
           (proc subtree)))
       tree))

; treat the tree as a sequence of sub-trees and map over it
(define (square-tree t)
  (tree-map sqr t))

(define t (cons (list 1 2) (list 3 4)))

(square-tree t)
