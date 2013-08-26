#lang racket


(define nil (list))


(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define t (cons (list 1 2) (list 3 4)))

(scale-tree t 2)
