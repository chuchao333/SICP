#lang racket

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))


(prime? 43)
(prime? 19)
(prime? 253)
(prime? 1397)
(prime? 255)

(display "-----------")
(newline)

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
