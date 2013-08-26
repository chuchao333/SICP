#lang racket


(define (gcd a b)
  (if (> b a)
    (gcd b a)
    (if (= b 0)
      a
      (gcd b (remainder a b)))))

; (define (make-rat n d)
;   (cons n d))

; reduce the rational number to lowest terms on construction
(define (make-rat n d)
  (let ((abs_n (abs n))
        (abs_d (abs d))
        (g (gcd (abs n) (abs d))))
    (if (< (* n d) 0)
      (cons (/ (- abs_n) g) (/ abs_d g))
      (cons (/ abs_n g) (/ abs_d g)))))

(define (numer rat) (car rat))
(define (denom rat) (cdr rat))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat rat)
  (newline)
  (display (numer rat))
  (display "/")
  (display (denom rat)))


(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(print-rat one-half)
(newline)

(print-rat (add-rat one-half one-third))
(print-rat (sub-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (div-rat one-half one-third))
(print-rat (add-rat one-third one-third))
(newline)

(print-rat (make-rat 0 2))
(newline)

(print-rat (make-rat 3 5))
(print-rat (make-rat -3 5))
(print-rat (make-rat 3 -5))
(print-rat (make-rat -3 -5))
(newline)
