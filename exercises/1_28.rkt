#lang racket

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-with-signal a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))

(define (expmod-with-signal base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-with-signal (expmod-with-signal base (/ exp 2) m) m))
        (else 
         (remainder (* base (expmod-with-signal base (- exp 1) m)) m))))

(define (square-with-signal a n)
  (if (and (not (= a 1))
           (not (= a (- n 1)))
           (= (remainder (* a a) n) 1))
      0
      (remainder (* a a) n)))


(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)

(newline)

(miller-rabin-test 7)
(miller-rabin-test 23)
(miller-rabin-test 103)
(miller-rabin-test 1009)
(miller-rabin-test 1019)
(miller-rabin-test 1000033)
