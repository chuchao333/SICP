#lang racket

(define tolerance 0.00001)

;; from the experiments, the nth root requires at least log2(n) average damps
(define (nth-root x n)
  (fixed-point
    ((repeated average-damp (floor (/ (log n) (log 2))))
    (lambda (y) (/ x (expt y (- n 1)))))
    1.0))

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- (/ x y) 1)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      ; (display next)
      ; (newline)
      (if (close-enough? next guess)
        next
        (try next))))
  (try first-guess))

(define (average-damp f)
  (define (average x y)
    (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))


(nth-root 16 4)
(nth-root 32 5)
(nth-root 64 6)
(nth-root 128 7)
(nth-root 256 8)
(nth-root 10000000000000000000000000000000000000000000000000000000000000000 64)
