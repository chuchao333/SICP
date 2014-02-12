#lang racket

(define nil (list))

;; this is also O(n) compared with the non-duplicate list
;; implementation, but as duplicate elements are allowed,
;; it may have more overhead as the size of the set grows.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (cons x set))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
  (append set1 set2))

(define set1 (list 1 2 3))
(define set2 (list 3 4 5))


(element-of-set? 1 set1)
(element-of-set? 1 set2)

(adjoin-set 4 set1)
(adjoin-set 4 set2)

(intersection-set set1 set2)
(union-set set1 set2)
