#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (display "current guess is: " )
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough next guess)
        next
        (try next))))
  (try first-guess))


; without average damping
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

; output below:
; current guess is: 2.0
; current guess is: 9.965784284662087
; current guess is: 3.004472209841214
; current guess is: 6.279195757507157
; current guess is: 3.759850702401539
; current guess is: 5.215843784925895
; current guess is: 4.182207192401397
; current guess is: 4.8277650983445906
; current guess is: 4.387593384662677
; current guess is: 4.671250085763899
; current guess is: 4.481403616895052
; current guess is: 4.6053657460929
; current guess is: 4.5230849678718865
; current guess is: 4.577114682047341
; current guess is: 4.541382480151454
; current guess is: 4.564903245230833
; current guess is: 4.549372679303342
; current guess is: 4.559606491913287
; current guess is: 4.552853875788271
; current guess is: 4.557305529748263
; current guess is: 4.554369064436181
; current guess is: 4.556305311532999
; current guess is: 4.555028263573554
; current guess is: 4.555870396702851
; current guess is: 4.555315001192079
; current guess is: 4.5556812635433275
; current guess is: 4.555439715736846
; current guess is: 4.555599009998291
; current guess is: 4.555493957531389
; current guess is: 4.555563237292884
; current guess is: 4.555517548417651
; current guess is: 4.555547679306398
; current guess is: 4.555527808516254
; current guess is: 4.555540912917957
; 4.555532270803653

; with average damping
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)

; output below:
; current guess is: 2.0
; current guess is: 5.9828921423310435
; current guess is: 4.922168721308343
; current guess is: 4.628224318195455
; current guess is: 4.568346513136242
; current guess is: 4.5577305909237005
; current guess is: 4.555909809045131
; current guess is: 4.555599411610624
; current guess is: 4.5555465521473675
; 4.555537551999825
