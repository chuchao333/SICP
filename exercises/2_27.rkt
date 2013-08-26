#lang racket

(define (deep-reverse items)
  (cond ((null? items) items)
        ((not (pair? (car items)))
         ; (append (deep-reverse (cdr items)) (list (car items))))
         (append (deep-reverse (cdr items)) (list (car items))))
        ; error 1: (else (append (deep-reverse (cdr items)) (deep-reverse (car items))))))
        ; error 2: (else (list (deep-reverse (cdr items)) (deep-reverse (car items))))))
        (else (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))))


(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2) (list 3 4) (list 5 6 (list 7 (list 8 9) 10))))

x
(reverse x)
(deep-reverse x)

y
(reverse y)
(deep-reverse y)
