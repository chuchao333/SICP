#lang racket

; k-term finite continued fraction
(define (cont-frac n d k)
  (define (frac i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (frac (+ i 1))))))
  (frac 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))


; k = 10 produce 0.6179775280898876
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                10)

; k = 11 produce 0.6180555555555556
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)


; TODO: implement a procedure to retry different k until the first
; one that produce the value accurate to 4 decimal.
