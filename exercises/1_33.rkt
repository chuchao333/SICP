#lang racket


(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
    null-value
    (if (filter a)
      (combiner (term a)
                (filtered-accumulate combiner null-value term (next a) next b filter))
      (filtered-accumulate combiner null-value term (next a) next b filter))))


(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (inc x) (+ x 1))

(define (identity x) x)

; return a procedure Integer -> Boolean
; that check if the given integer is relative prime with n
(define (relative-prime? n)
  (define (f a) (= (gcd n a) 1))
  f)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(filtered-accumulate + 0 square 2 inc 10 prime?)
(filtered-accumulate * 1 identity 1 inc 10 (relative-prime? 10))
