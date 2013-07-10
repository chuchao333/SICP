#lang racket

(define (search-for-primes start end)
  (if (even? start)
    (search-for-primes (+ start 1) end)
    (cond ((< start end) (timed-prime-test start)
                         (search-for-primes (+ start 2) end)))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
         (report-prime (- (current-inexact-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))


; 1009 *** 0.02294921875
; 1013 *** 0.0009765625
; 1019 *** 0.0009765625
; 10007 *** 0.001953125
; 10009 *** 0.002197265625
; 10037 *** 0.001953125
; 100003 *** 0.005859375
; 100019 *** 0.005859375
; 100043 *** 0.005859375
; 1000003 *** 0.01904296875
; 1000033 *** 0.01904296875
; 1000037 *** 0.01904296875

(timed-prime-test 1009)
(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

; 1009 *** 0.0009765625
; 1013 *** 0.0009765625
; 1019 *** 0.0009765625
; 10007 *** 0.001953125
; 10009 *** 0.001953125
; 10037 *** 0.001953125
; 100003 *** 0.00390625
; 100019 *** 0.005126953125
; 100043 *** 0.00390625
; 1000003 *** 0.010009765625
; 1000033 *** 0.010009765625
; 1000037 *** 0.010009765625
