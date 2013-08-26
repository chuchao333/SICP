#lang racket


(define (square-list items)
  (if (null? items)
    items
    (cons (sqr (car items))
          (square-list (cdr items)))))


(define (square-list-1 items)
  (map (lambda (x) (* x x)) items))


(square-list-1 (list 1 2 3 4 5))
(square-list (list 1 2 3 4 5))
