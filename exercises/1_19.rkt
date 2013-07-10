#lang racket

; use the matrix multiplication to represent the transformation

;                       0 1
; (fib(n-1), fib(n)) *        = (fib(n), fib(n-1) + fib(n)) = (fib(n), fib(n+1))
;                       1 1

; a <- a + b
; b <- a

; the state transformation above is just a special case of the below one when
; p = 0 and q = 1

; a <- bq + aq + ap
; b <- bp + aq

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter
           a
           b
           (+ (square p) (square q))
           (+ (square q) (* p q 2))
           (/ count 2)))
  (else (fib-iter
          (+ (* b q) (* a q) (* a p))
          (+ (* b p) (* a q))
          p
          q
          (- count 1)))))

(define (square x) (* x x))


(fib 5)
(fib 7)
(fib 8)
