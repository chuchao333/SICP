#lang racket

(define (mul a b)
  (cond ((= b 0) 0)
        ((even? b) (mul (double a) (halve b)))
        (else (+ a (mul a (- b 1))))))

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(mul 3 4)
(mul 4 3)
(mul 2 5)
