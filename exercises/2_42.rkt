#lang racket


(define nil (list))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

; flatmap is actually the combination of mapping and accumulating
; with append.
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; represent the positions as a sequnce of:
; ((n, rown), (n-1, row(n-1)), ..., (1, row1)),
; which means a queen is placed in the row 'row1' in col 1, 'row2' in
; col 2 and so on. The redundant col numbers here are to make the impl
; of safe? easier.

; the empty-board is just a empty list.
(define empty-board nil)

(define (adjoin-position new-row col rest-of-queens)
  (cons (list col new-row) rest-of-queens))

(define (safe? k positions)
  (and (safe-row? (cadr (car positions)) (map cadr (cdr positions)))
       (safe-diagonal? (car positions) (cdr positions))))

(define (safe-row? row others)
  (null? (filter (lambda (x) (= x row)) others)))

(define (safe-diagonal? cur prev-positions)
  (null? (filter (lambda (x)
                   (= (abs (- (car x) (car cur)))
                      (abs (- (cadr x) (cadr cur)))))
                 prev-positions)))

; TODO: redo this from scratch to really understand it
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter (lambda (positions) (safe? k positions))
              (flatmap (lambda (rest-of-queens)
                         (map (lambda (new-row)
                                (adjoin-position new-row
                                                 k
                                                 rest-of-queens))
                              (enumerate-interval 1 board-size)))
                       (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (print-queens sols n)
  (map (lambda (x, (newline))
         (map (lambda (cur)
                (if (= x (cadr cur))
                  (display "*")
                  (display "-")))
              (reverse sols)))
       (enumerate-interval 1 n)))

; ---------------------------------------------------
(queens 8)
; (map (lambda (queens) (print-queens queens 4)) (queens 4))
; (queens 8)
