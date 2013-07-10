#lang racket

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (average a b)
    (/ (+ a b) 2))

  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) 0.001))
  (sqrt-iter 1.0))


(sqrt 9)
(sqrt (+ 100 37) )
(sqrt (+ (sqrt 2) (sqrt 3)))

(sqrt 0.001)
(sqrt 1e13)
