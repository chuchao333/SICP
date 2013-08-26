#lang racket


(define nil (list))


(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              sequence))

(map sqr (list 1 2 3))
(map (lambda (x) (- 0 x)) (list 1 2 3))


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2) (list 3 (list 4)))
(append (list 1 2) (list 3))


(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

(length (list 1 3 5))
(length (list))
