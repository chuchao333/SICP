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


(fold-right / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))

(fold-left / 1 (list 1 2 3))
(fold-left list nil (list 1 2 3))



; fold-left and fold-right will produce the same results
; when "a op b" equals "b op a"
(fold-right + 0 (list 1 2 3))
(fold-left + 0 (list 1 2 3))
(fold-right * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))
