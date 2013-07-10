#lang racket

(define (count-change amount)
  ; different from the example in book, here
  ; consider the coins as arranged in order from smallest to largest
  (cc amount 1))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 6)) 0)
  (else (+ (cc amount (+ kinds-of-coins 1))
           (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


(count-change 100)
