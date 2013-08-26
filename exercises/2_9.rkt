#lang racket

(require test-engine/racket-tests)
(require rackunit)

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define (print-interval i)
  (display "(")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display ")")
  (newline))

(define i1 (make-interval 2 8))
(define i2 (make-interval 1 4))

(print-interval (add-interval i1 i2))
(print-interval (sub-interval i1 i2))
(print-interval (mul-interval i1 i2))
(print-interval (div-interval i1 i2))


(check-expect (width-interval (add-interval i1 i2))
              (+ (width-interval i1)
                 (width-interval i2)))

(check-expect (width-interval (sub-interval i1 i2))
              (- (width-interval i1)
                 (width-interval i2)))

(check-not-eq? (width-interval (mul-interval i1 i2))
               (* (width-interval i1) (width-interval i2)))