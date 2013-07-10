#lang racket

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))


(define (improve guess x)
  (average guess (/ x guess)))


(define (average a b)
  (/ (+ a b) 2))


;(define (good-enough? guess x)
;  (< (abs (- x (square guess))) 0.001))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) 0.001))


(define (square x)
  (* x x))


(define (sqrt x)
  (sqrt-iter 1.0 x))


(sqrt 9)
(sqrt (+ 100 37) )
(sqrt (+ (sqrt 2) (sqrt 3)))

(sqrt 0.001)
(square (sqrt 0.001))
(sqrt 1e13)
