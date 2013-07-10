#lang racket


(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter cur result)
    (if (> cur b)
      result
      (iter (next cur) (* (term cur) result))))
  (iter a 1))

(define (factorial n)
  (define (identity x) x)
  (product identity 1 inc n))

(define (inc x) (+ x 1))


(product (lambda (x) x) 1 (lambda (x) (+ x 1)) 6)
(product-iter (lambda (x) x) 1 (lambda (x) (+ x 1)) 6)
(factorial 6)
(factorial 10)
(product (lambda (x) (* x x)) 1 (lambda (x) (+ x 1)) 5)
(product-iter (lambda (x) (* x x)) 1 (lambda (x) (+ x 1)) 5)

;; compute the approximations to PI using the formula:
;; PI   2*4*4*6*6*8...
;; -- = --------------
;; 4    3*3*5*5*7*7...

;; here, term is (2n/(2n + 1) * 2(n+1) /(2n + 1)), n = 1, 2, 3, ...
(/
  (product-iter
    (lambda (n)
      (* (* 2 n) (* 2 (+ n 1.0))))
    1
    inc
    50)
  (product-iter
    (lambda (n)
      (* (+ (* 2 n) 1.0) (+ (* 2 n) 1.0)))
    1
    inc
    50)
)
