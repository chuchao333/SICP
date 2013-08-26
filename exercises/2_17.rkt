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

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

#|
; in terms of list-ref and length
(define (last-pair items)
  (if (null? items)
    (error "illegal argument, should not be empty.")
    (list (list-ref items (- (length items) 1)))))

; recursive version
(define (last-pair items)
  (if (null? items)
    (error "illegal argument, should not be empty.")
    (if (null? (cdr items))
      items
      (last-pair (cdr items)))))
|#

; iterative version
(define (last-pair items)
  (define (iter items cur-cdr)
    (if (null? cur-cdr)
      items
      (iter (cdr items) (cdr (cdr items)))))
  (if (null? items)
    (error "illegal argument, should not be empty.")
    (iter items (cdr items))))


(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7 9 11))

(last-pair squares)
(last-pair odds)
