#lang racket

(define (mul a b)
  (mul-iter a b 0))

(define (mul-iter a b sum)
  (cond ((= b 0) sum)
        ((even? b) (mul-iter (double a) (halve b) sum))
        (else (mul-iter a (- b 1) (+ a sum)))))


(define (double x) (* x 2))
(define (halve x) (/ x 2))

(mul 3 4)
(mul 4 3)
(mul 2 5)
