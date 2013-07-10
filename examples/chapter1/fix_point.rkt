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


; This results in an infinite loop since the guesses are:
; y1 -> x/y1 -> x / (x/y1) -> y1 ...
(define (sqrt-bad x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

; average damping - averaging successive approximations to a solution, can aid
; the convergence.
(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))


; since the golden-ratio x satisfies that x^2 = x + 1
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))


; (fixed-point cos 1.0)
; (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(sqrt 4)
(sqrt 6)
(sqrt 8)
(sqrt 9)

(golden-ratio)
