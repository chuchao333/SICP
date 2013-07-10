#lang racket

; both row and col indexes start at 0
(define (pascal-triangle row col)
  (cond ((= col 0) 1)
        ((= row col) 1)
  (else (+ (pascal-triangle (- row 1) (- col 1))
           (pascal-triangle (- row 1) col)))))


(pascal-triangle 4 1)
(pascal-triangle 4 2)
(pascal-triangle 5 2)
(pascal-triangle 6 2)
(pascal-triangle 6 5)
(pascal-triangle 6 6)
