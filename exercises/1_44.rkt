#lang racket


(define (n-fold-smooth f n)
  (repeated (smooth f) n))

(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x)
      (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))

((n-fold-smooth (lambda (x) (/ 1.0 x)) 3) 6)
((lambda (x) (/ 1.0 x)) 6)
