#lang racket

; variables are symbols
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? expression num)
  (and (number? expression) (= expression num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 1) b)
        (else (list '** b e))))

; a sum is an expression, in which there is a top level '+'
(define (sum? x)
  (memq '+ x))

; The addend is the sub expression before the (first) '+' 
(define (addend s)
  (if (eq? (cadr s) '+)
    (car s)
    (take s (- (length s) (length (memq '+ s))))))

; The augend is the sub expression after the (first) '+'
(define (augend s)
  (let ((len (length s))
        (sublen (length (memq '+ s))))
    (if (= sublen 2)
      (car (drop s (+ (- len sublen) 1)))
      (drop s (+ (- len sublen) 1)))))

; a product is a list whose second element is the symbol *
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

; the multiplier is the first item of the product list
(define (multiplier p) (car p))

; the multiplicand is the product of the rest of the terms
(define (multiplicand p)
  (if (null? (cdddr p))
    (caddr p)
    (cddr p)))

; represent the exponentiation with the symbol '**'
(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

; the base is the first item of the exponentiation list
(define (base e) (car e))

; the exponent is the third item of the exponentiation list
(define (exponent e) (caddr e))


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
        ((exponentiation? expression)
         (make-product (exponent expression)
                       (make-product (make-exponentiation (base expression)
                                                          (- (exponent expression) 1))
                                     (deriv (base expression) variable))))
        (else (error "unknown expression type - DERIV" expression))))


(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x * 3 + 5 * (x + y + 2)) 'x)
(deriv '(x * 3 + 5 * x + y + 2 * x) 'x)
(deriv '(x * 3 + 5 * (x + y + 2 * x)) 'x)
(deriv '((x * 3 + y + 1) * (5 + 2 * x)) 'x)
