#lang racket


(define nil (list))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(filter odd? (list 1 2 3 4 5))


(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))


(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
; the result is still '(1 2 3 4 5), not quite straightforward
(accumulate cons nil (list 1 2 3 4 5))


(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)


; enumerate the leaves of a tree
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ; this is also correct
        ; ((not (pair? tree)) (list tree))
        ((not (pair? (car tree)))
         (cons (car tree) (enumerate-tree (cdr tree))))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


(enumerate-tree (list 1 (list 2 (list 3 4)) 5))


(define (sum-odd-squares tree)
  (accumulate +
              0
              (map sqr (filter odd? (enumerate-tree tree)))))

(sum-odd-squares (cons (list 1 2 3) (list 4 5 6)))


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
  (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib (enumerate-interval 0 n)))))

(even-fibs 5)


(define (list-fib-squares n)
  (accumulate cons
              nil
              (map sqr (map fib (enumerate-interval 0 n)))))

(list-fib-squares 10)
