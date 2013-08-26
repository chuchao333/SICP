#lang racket

(define (scale-list items factor)
  (if (null? items)
    items
    (cons (* (car items) factor)
          (scale-list (cdr items) factor))))


(scale-list (list 1 2 3 4 5) 10)


; the limitation here is that the 'proc' can only be an unary function
(define (map proc items)
  (if (null? items)
    items
    (cons (proc (car items))
          (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))


; scale-list in terms of map
(define (scale-list-1 items factor)
  (map (lambda (x) (* x factor)) items))


(scale-list-1 (list 1 2 3 4 5) 10)
