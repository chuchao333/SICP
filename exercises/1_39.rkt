#lang racket

; if i = 1, (n i) = -x else (n i) = -x^2
; (d i) = 2 * i - 1
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                           x
                           (- 0(* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))


; tan (pi / 8)
(tan-cf 0.4 10)

; tan (pi / 4)
(tan-cf 0.7853981633974483 10)

