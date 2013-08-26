#lang racket


(define (same-parity . seq)
  (if (null? seq)
    seq
    (let ((parity (remainder (car seq) 2)))
      (define (iter items p)
        (if (null? items)
          items
          (if (= (remainder (car items) 2) p)
            (cons (car items) (iter (cdr items) p))
            (iter (cdr items) p))))
      (iter seq parity))))


(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity)
