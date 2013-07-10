#lang racket

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (cbrt-iter (improve guess x) x)))


(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))


(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) 0.001))


(define (square x)
  (* x x))


(define (cbrt x)
  (cbrt-iter 1.0 x))


(cbrt 27)
(cbrt (+ 100 37) )
(cbrt (+ (cbrt 2) (cbrt 3)))

(cbrt 0.001)
(* (square (cbrt 0.001)) 0.001)
(cbrt 1e15)
