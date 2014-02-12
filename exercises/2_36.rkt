#lang racket


(define nil (list))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
    ; all the sub-sequences are all empty now
    nil
    (cons (accumulate op initial (map car seqs))
          (accumulate-n op initial (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)
(accumulate-n * 1 s)

; this is a 'zip' of the s
(accumulate-n cons nil s)
