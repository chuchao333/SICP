#lang racket

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


(define (make-center-with c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-with c (* c (/ p 100.0))))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(define (div-interval x y)
  (if (> (* (lower-bound y) (upper-bound y)) 0)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))
    (error "divided by interval containing 0.")))


(define (print-interval i)
  (display "(")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display ")")
  (newline))

(define i1 (make-interval 2 8))
(define i2 (make-interval 1 4))
(define i3 (make-center-percent 5 1))

(print-interval i1)
(print-interval i3)
(percent i3)
