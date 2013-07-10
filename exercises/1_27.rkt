#lang racket

(define (fermat-test n)
  (fermat-test-iter n 2))

(define (fermat-test-iter n a)
  (cond ((= n a) true)
        ((not (= (expmod a n n) a)) false)
  (else (fermat-test-iter n (+ a 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
  (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x) (* x x))


; Carmichael numbers
(fermat-test 561)
(fermat-test 1105)
(fermat-test 1729)
(fermat-test 2465)
(fermat-test 2821)
(fermat-test 6601)

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
