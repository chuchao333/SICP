#lang racket


(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))


; if    i % 3 == 2, (d i) = 2 * ((i + 1) / 3)
; else  (d i) = 1
(+ (cont-frac (lambda (i) 1.0)
              (lambda (i) (let ((r (remainder i 3)))
                            (if (= r 2)
                              (* 2.0 (/ (+ i 1) 3))
                              1.0)))
              10)
   2)
