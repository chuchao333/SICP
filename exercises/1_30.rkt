#lang racket


(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
(sum-iter (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)

(sum (lambda (x) (* x x)) 1 (lambda (x) (+ x 1)) 5)
(sum-iter (lambda (x) (* x x)) 1 (lambda (x) (+ x 1)) 5)
