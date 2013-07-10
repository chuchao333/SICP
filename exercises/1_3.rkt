#lang racket

(define (square x) (* x x))

(define (sum-of-squares a b) (+ (square a) (square b)))

(define (min a b)
  (if (< a b)
    a
    b))

(define (max a b)
  (if (< a b)
    b
    a))

(define (sum-of-squares-of-2-largest a b c)
  (sum-of-squares (max a b) (max c (min a b))))

(sum-of-squares-of-2-largest 3 4 5)
(sum-of-squares-of-2-largest 2 5 7)
