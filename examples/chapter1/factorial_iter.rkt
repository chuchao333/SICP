#lang racket

; product = product * counter
; counter = counter + 1

(define (factorial n)
  (define (fact-iter product counter)
    (if (> counter n)
      product
      (fact-iter (* product counter) (+ counter 1))))
  (fact-iter 1 1))

(factorial 3)
(factorial 5)
