#lang racket

; variables are symbols
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? expression num)
  (and (number? expression) (= expression num)))

; (define (make-sum a1 a2) (list '+ a1 a2))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

; (define (make-product m1 m2) (list '* m1 m2))
(define (make-product m1 m2) (list '* m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; a sum is a list whose first element is the symbol +
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

; The addend is the second item of the sum list
(define (addend s) (cadr s))

; The augend is the third item of the sum list
(define (augend s) (caddr s))

; a product is a list whose first element is the symbol *
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

; the multiplier is the second item of the product list
(define (multiplier p) (cadr p))

; the multiplicand is the third item of the product list
(define (multiplicand p) (caddr p))


(define (deriv expression variable)
  (cond ((number? expression) 0)
        ((variable? expression)
         (if (same-variable? expression variable)
           1
           0))
        ((sum? expression)
         (make-sum (deriv (addend expression) variable)
                   (deriv (augend expression) variable)))
        ((product? expression)
         (make-sum (make-product (multiplier expression)
                                 (deriv (multiplicand expression) variable))
                   (make-product (multiplicand expression)
                                 (deriv (multiplier expression) variable))))
        (else (error "unknown expression type - DERIV" expression))))


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)
