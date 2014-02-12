#lang racket

; (a b c)
(list 'a 'b 'c)

; ((george))
(list (list 'george))

; ((y1 y2))
(cdr '((x1 x2) (y1 y2)))

; (y1 y2)
(cadr '((x1 x2) (y1 y2)))

; unbound identifier 'a' (I was wrong)
; correct answer is #f
(pair? (car '(a short list)))

; #f
(memq 'red '((red shoes) (blue socks)))

; (red shots blue socks)
(memq 'red '(red shoes blue socks))
