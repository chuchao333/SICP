#lang racket

; use successive squaring
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
  (else (* b (square (fast-expt b (/ (- n 1) 2)))))))

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))


(fast-expt 2 10)
(fast-expt 5 3)
