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

(define (make-triple-sum triple)
  (let ((p1 (car triple))
        (p2 (cadr triple))
        (p3 (cadr (cdr triple))))
    (list p1 p2 p3 (+ p1 p2 p3))))

(define (unique-triples n)
  (flatmap (lambda (cur-i)
             (flatmap (lambda (cur-j)
                    (map (lambda (cur-k)
                           (list cur-i cur-j cur-k))
                         (enumerate-interval 1 (- cur-j 1))))
                  (enumerate-interval 1 (- cur-i 1))))
           (enumerate-interval 1 n)))


(define (s-sum-triples n s)
  (map make-triple-sum
       (filter (lambda (t)
                 (= (+ (car t)
                       (cadr t)
                       (cadr (cdr t)))
                    s))
               (unique-triples n))))


; ---------------------------------------------------
(unique-triples 6)
(s-sum-triples 6 9)
