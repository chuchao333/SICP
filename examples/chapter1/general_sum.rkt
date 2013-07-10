#lang racket

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum-cubes a b)
  (define (cube x) (* x x x))
  (sum cube a inc b))

(define (sum-integers a b)
  (define (identity x) x)
  (sum identity inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (inc n) (+ n 1))


(sum-cubes 1 8)
(* 8 (pi-sum 1 1000))
(integral (lambda (x) (* x x x)) 0 1 0.01)
(integral (lambda (x) (* x x x)) 0 1 0.001)
; (integral cube 0 1 0.001)
