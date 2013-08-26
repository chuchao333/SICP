#lang racket


; alternative procedural representation of pairs
; cons is a procedure which returns a lambda function, the lambda
; function accepts a single argument m, which is also a procedure,
; the result of the lambda is the result of applying m to the original
; arguments of cons (x and y)
(define (cons x y)
  (display "cons")
  (newline)
  (lambda (m) (m x y)))

; z is a lambda returned by cons
; here m is also passed as a lambda which simply returns the first argument
; of the given two arguments:
; (lambda (p q) p)
(define (car z)
  (display "car")
  (newline)
  (z (lambda (p q) p)))


; z is a lambda returned by cons
; here m is also passed as a lambda which simply returns the second argument
; of the given two arguments:
; (lambda (p q) q)
(define (cdr z)
  (display "cdr")
  (newline)
  (z (lambda (p q) q)))


(car (cons 1 2))
(cdr (cons 1 2))
