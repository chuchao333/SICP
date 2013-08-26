#lang racket

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))
#|
; the recursive version
(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))
|#

; the iterative version
(define (length items)
  (define (length-iter items count)
    (if (null? items)
      count
      (length-iter (cdr items) (+ 1 count))))
   (length-iter items 0))

(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7 9 11))


(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(list-ref squares 3)
(length odds)
(length squares)
(append squares odds)
