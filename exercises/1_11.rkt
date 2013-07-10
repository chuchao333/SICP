#lang racket

(define (f-recursive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
  (else (+ (f-recursive (- n 1))
           (* (f-recursive (- n 2)) 2)
           (* (f-recursive (- n 3)) 3)))))


(define (f-iterative n)
  (f-iter 0 1 2 n))

(define (f-iter a b c count)
  (cond ((= count 0) a)
        ((= count 1) b)
        ((= count 2) c)
  (else (f-iter b c (+ c (* b 2) (* a 3)) (- count 1)))))

(f-recursive 0)
(f-recursive 1)
(f-recursive 2)
(f-recursive 3)
(f-recursive 4)
(f-recursive 5)

(f-iterative 0)
(f-iterative 1)
(f-iterative 2)
(f-iterative 3)
(f-iterative 4)
(f-iterative 5)
