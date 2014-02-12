#lang racket

(define nil (list))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((< x (car set)) false)
        ((= x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (cond ((element-of-set? x set) set)
        ((null? set) (list x))
        (else (if (< x (car set))
                (cons x set)
                (cons (car set) (adjoin-set x (cdr set)))))))


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    nil
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-set (cdr set1) set2)))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((> x1 x2)
             (intersection-set set1 (cdr set2)))))))


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr set1)
                                           (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr set1)
                                           set2)))
                      ((> x1 x2)
                       (cons x2 (union-set set1
                                           (cdr set2)))))))))


(define set1 (list 1 2 3))
(define set2 (list 3 4 5))


(element-of-set? 1 set1)
(element-of-set? 1 set2)

(adjoin-set 5 set1)
(adjoin-set 1 set2)

(intersection-set set1 set2)
(union-set set1 set2)
