#lang racket


(define nil (list))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

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

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (cur-i)
                          (map (lambda (cur-j)
                                 (list cur-i cur-j))
                               (enumerate-interval 1 (- cur-i 1))))
                        (enumerate-interval 1 n)))))


(define (remove x seq)
  (filter (lambda (cur) (not (= cur x))) seq))

(define (permutations s)
  (if (null? s)         ; empty set?
    (list nil)          ; sequence containing empty set
    (flatmap (lambda (x)
               (map (lambda (p)
                      (cons x p))
                    (permutations (remove x s))))
             s)))


; --------------------------------------------------------------

(accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j)
                          (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 6)))

(flatmap (lambda (cur-i)
           (map (lambda (cur-j)
                  (list cur-i cur-j))
                (enumerate-interval 1 (- cur-i 1))))
         (enumerate-interval 1 6))

(prime-sum-pairs 6)

(permutations (list 1 2 3))

; the following examples showed clearly the difference between map and flatmap
(map list (list 1 2 3))
(flatmap list (list 1 2 3))

(map (lambda (x) (list x (list x))) (list 1 2 3))
(flatmap (lambda (x) (list x (list x))) (list 1 2 3))
