#lang racket


(define nil (list))


; compute the sum of the squares of the leaves that are odd
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (sqr tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(sum-odd-squares (cons (list 1 2 3) (list 4 5 6)))


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
  (else (+ (fib (- n 1)) (fib (- n 2))))))

; constructs a list of all the even fibonacci numbers fib(k), where k is
; less than or equal to a given integer n.
;
; note how the recursive function 'next' is shaped.
(define (even-fibs n)
  (define (next k)
    (if (> k n)
      nil
      (let ((f (fib k)))
         (if (even? f)
           (cons f (next (+ k 1)))
           (next (+ k 1))))))
  (next 0))


(even-fibs 5)
