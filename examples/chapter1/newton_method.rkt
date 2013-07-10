#lang racket

(define (average-damp f)
  (define (average a b)
    (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

(define (square x) (* x x))

;; demo the use of average-damp
((average-damp square) 10)


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

; the Newton's method to 
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

; sqrt with Newton's method
; the solution of g(x) = 0 is a fix point of (newton-transform g)
; so in order to get sqrt, here g(x) = y^2 - x
(define (sqrt-with-newton x)
  (fixed-point (newton-transform (lambda (y) (- (square y) x))) 1.0))

(sqrt 4)
(sqrt-with-newton 4)
(sqrt 9)
(sqrt-with-newton 9)

(cube-root 8)
(cube-root 27)

((deriv cube) 5)
