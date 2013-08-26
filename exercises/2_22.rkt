#lang racket


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (sqr (car things))
                  answer))))
  (iter items (list)))


(square-list (list 1 3 5))
