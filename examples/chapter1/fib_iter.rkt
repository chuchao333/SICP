#lang racket

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= 0 count)
    b
    (fib-iter (+ a b) a (- count 1))))

(fib 5)
(fib 7)
(fib 8)

