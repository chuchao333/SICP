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

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 4 5 6))


(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(matrix-*-vector (list (list 1 2 3)
                       (list 4 5 6))
                 (list 2 2 2))


(define (transpose mat) (accumulate-n cons nil mat))

(transpose (list (list 1 2 3) (list 4 5 6)))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (matrix-*-vector cols m-row))
         m)))

(matrix-*-matrix (list (list 1 2 3) (list 4 5 6))
                 (list (list 1 2) (list 1 2) (list 1 2)))
