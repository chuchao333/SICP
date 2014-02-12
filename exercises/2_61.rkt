#lang racket

(define (adjoin-set x set)
  (cond ((element-of-set? x set) set)
        ((null? set) (list x))
        (else (if (< x (car set))
                (cons x set)
                (cons (car set) (adjoin-set x (cdr set)))))))
