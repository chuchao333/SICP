#lang racket

(define (average-damp f)
  (define (average a b)
    (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

(define (square x) (* x x))

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

;; re-write the square-root procedure
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

;; cube root, similar with above
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))


(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x)
  (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a (square x)) (* b x) c)))

; zero of x^3 + x^2 + x + 1
; should be -1
(newton-method (cubic 1 1 1) 1.0)
