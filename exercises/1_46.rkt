#lang racket

(define tolerance 0.00001)

(define (iterative-improve enough? improve)
  (define (try guess)
    (let ((next-guess (improve guess)))
      (if (enough? guess next-guess)
        guess
        (try next-guess))))
  try)

(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

(define (sqrt x)
  ((iterative-improve
      close-enough?
      (lambda (y) (average y (/ x y))))
      1.0))

(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (/ (- x y) y)) tolerance))


(sqrt 4)
(sqrt 6)
(sqrt 9)

(fixed-point cos 1.0)
