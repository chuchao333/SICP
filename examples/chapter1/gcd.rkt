#lang racket

; assume that a is the larger value
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(gcd 206 40)
(gcd 20 10)
