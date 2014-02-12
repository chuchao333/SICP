#lang racket


(define nil (list))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)


(define (fold-left op initial sequence)
  (define (iter left cur)
    (if (null? left)
      cur
      (iter (cdr left) (op cur (car left)))))
  (iter sequence initial))


(define (reverse-1 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))


(define (reverse-2 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))


(reverse-1 (list 1 3 5))
(reverse-2 (list 2 4 6))
