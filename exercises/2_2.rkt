#lang racket


(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (mid-segment s)
  (define (average a b)
    (/ (+ a b) 2))
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))


(define p1 (make-point 1 3))
(define p2 (make-point 3 5))

(print-point (mid-segment (make-segment p1 p2)))
(newline)
