#lang racket


(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (inc x) (+ x 1))


((repeated square 2) 5)
