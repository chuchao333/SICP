#lang racket

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))


(define (improve guess x)
  (average guess (/ x guess)))


(define (average a b)
  (/ (+ a b) 2))


(define (good-enough? guess x)
  (< (abs (- x (square guess))) 0.001))


(define (square x)
  (* x x))


(define (sqrt x)
  (sqrt-iter 1.0 x))


(sqrt 9)
(sqrt (+ 100 37) )
(sqrt (+ (sqrt 2) (sqrt 3)))

; for small values, the absolute tolerance 0.001 is too large
(sqrt 0.001)
(square (sqrt 0.001))
; for large values, due to the precision limitation of float point
; representation, the guess could not be refined to a value that can be
; represented within the tolerance.
; (sqrt 1e13)
