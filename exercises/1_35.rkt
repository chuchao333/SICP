#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough next guess)
        next
        (try next))))
  (try first-guess))


; since the golden-ratio x satisfies that x^2 = x + 1
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))


(golden-ratio)


