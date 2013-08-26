#lang racket

(define nil (list))

; the subset of s(a, b, c, ...) is the union of:
; 1) subset s'(b c, ...)
; 2) a new set add a to each element of s'

; in other words:
; the subset of s(a, b, c, ...) comprised of two parts:
; 1) the permutations of s without the element a: this is a sub-problem to get s'
; 2) the permutations of s with the element a: this is the new set that adding
;    a to each of the element in s'.
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x)
                          (cons (car s) x))
                        rest)))))

(subsets (list 1 2 3))
