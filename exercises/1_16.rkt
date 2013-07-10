#lang racket

; the hint is quite useful

; in general, the technique of defining an invariant quantity that remains
; changed from state to state is a powerful way to think about the design
; of iterative algorithms.

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n product)
  (cond ((= n 0) product)
        ((even? n) (fast-expt-iter (square b) (/ n 2) product))
        (else (fast-expt-iter b (- n 1) (* b product)))))

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))


(fast-expt 2 10)
(fast-expt 5 3)
