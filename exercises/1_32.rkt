#lang racket


(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter cur result)
    (if (> cur b)
      result
      (iter (next cur) (combiner (term cur) result))))
  (iter a null-value))

(define (sum term a next b)
  ; (accumulate + 0 term a next b))
  (accumulate-iter + 0 term a next b))

(define (product term a next b)
  ; (accumulate * 1 term a next b))
  (accumulate-iter * 1 term a next b))


(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 6)
(product (lambda (x) x) 1 (lambda (x) (+ x 1)) 6)
(sum (lambda (x) (* x x)) 1 (lambda (x) (+ x 1)) 5)
(product (lambda (x) (* x x)) 1 (lambda (x) (+ x 1)) 5)
