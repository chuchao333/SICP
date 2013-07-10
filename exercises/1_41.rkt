#lang racket

;; Note: it's a little confusing at first, 'double' is not apply the function
;; 'f' and double the result, it's 'DOUBLE' the application of the function
;; 'f'.
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

((double inc) 1)
(((double double) inc) 1)

; this will get 21
(((double (double double)) inc) 5)
