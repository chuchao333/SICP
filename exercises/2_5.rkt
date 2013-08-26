#lang racket

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))


(define (count-factor-num n factor)
  (define (iter n cur)
    (if (= (remainder n factor) 0)
      (iter (/ n factor) (+ cur 1))
      cur))
  (iter n 0))


(define (car z)
  (count-factor-num z 2))

(define (cdr z)
  (count-factor-num z 3))


(car (cons 3 5))
(cdr (cons 8 10))
