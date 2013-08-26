#lang racket


(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (distance p1 p2)
  (sqrt (+ (sqr (- (x-point p1) (x-point p2)))
           (sqr (- (y-point p1) (y-point p2))))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (length-segment s)
  (distance (start-segment s) (end-segment s)))


; represent the rectangle with two segments (left, top)
#|
(define (make-rectangle left top) (cons left top))
(define (left-rectangle rec) (car rec))
(define (top-rectangle rec) (cdr rec))
; length-rectangle is the longer side
(define (length-rectangle rec)
  (let ((left-length (length-segment (left-rectangle rec)))
        (top-length (length-segment (top-rectangle rec))))
    (max left-length top-length)))
; width-rectangle is the shorter side
(define (width-rectangle rec)
  (let ((left-length (length-segment (left-rectangle rec)))
        (top-length (length-segment (top-rectangle rec))))
    (min left-length top-length)))

(define rec1
  (make-rectangle (make-segment (make-point 1 1) (make-point 1 3))
                  (make-segment (make-point 1 1) (make-point 5 1))))

(perimeter-rectangle rec1)
(area-rectangle rec1)
|#

(define (perimeter-rectangle rec)
  (* 2 (+ (width-rectangle rec) (length-rectangle rec))))

(define (area-rectangle rec)
  (* (width-rectangle rec) (length-rectangle rec)))

; represent the rectangle with 3 points (tl, tr, bl)
(define (make-rectangle tl tr bl) (cons tl (cons tr bl)))
(define (top-left rec) (car rec))
(define (top-right rec) (car (cdr rec)))
(define (bottom-left rec) (cdr (cdr rec)))

(define (length-rectangle rec)
  (let ((tl-to-tr (distance (top-left rec) (top-right rec)))
        (tl-to-bl (distance (top-left rec) (bottom-left rec))))
    (max tl-to-tr tl-to-bl)))

(define (width-rectangle rec)
  (let ((tl-to-tr (distance (top-left rec) (top-right rec)))
        (tl-to-bl (distance (top-left rec) (bottom-left rec))))
    (min tl-to-tr tl-to-bl)))

(define rec2
  (make-rectangle (make-point 1 3)
                  (make-point 5 3)
                  (make-point 1 1)))


(perimeter-rectangle rec2)
(area-rectangle rec2)
