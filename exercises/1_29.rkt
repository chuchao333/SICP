#lang racket

(define (simpson-rule f a b n)
  ; still use 'define' since we haven't learned 'let' till now
  (define h (/ (- b a) n))
  ; the procedure to compute 'yk', the helper method used by 'term'
  (define (y k) (f (+ a (* k h))))
  ; get the 'next and 'term for the sum procedure
  (define (next k) (+ k 1))
  (define (term k)
    (* (cond ((or (= k 1) (= k n)) 1)
             ((even? k) 2)
             ((odd? k) 4))
       (y k)))
  (/ (* h (sum term 0 next n)) 3))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (cube x) (* x x x))

(simpson-rule cube 0 1 100.0)
(simpson-rule cube 0 1 1000.0)
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;; the output:
; 0.24999998999999987
; 0.24999999999900027
; 0.24998750000000042
; 0.249999875000001
