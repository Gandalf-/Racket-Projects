#lang racket

(require "readword.rkt" )

; Definitions
(define thousandth 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501)

(define golden 1.6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374)

(define minus-golden -0.6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374)

(define sqrt-five 2.236067977499789696409173668731276235440618359611525724270897245410520925637804899414414408378782274969508176150)

; Prints x to a file
(define (print-this x name)
  (call-with-output-file name
    (lambda (output-port)
      (display x output-port))))

; Approximating 'at'th fibonacci number above 2000
; print result to file
(define (fibonacci-approx-file at file-name)
  (print-this (* thousandth (- at 1001))
              file-name) )

;(fibonacci-approx-file 1000000 "Data/million-approx.txt")


; Approximating 'at'th fibonacci number above 2000
; but less than million. 
(define (fibonacci-approx at)
  (displayln 
    (inexact->exact
      (* thousandth 
         (expt 
           golden 
           (- at 1000))))))


;Using the phi, sqrt 5 formula LIMIT < 10k
(define (fibonacci-formula n)
  (displayln
    (inexact->exact 
      (/ (- (expt golden n)
            (expt minus-golden n))
         sqrt-five))))



;Whats the relationship between the n'th fibo num and it's length?

;Very accurate!
(define (expected-simple place)
  (if (or (= place 1) (= place 2))
    1.0
    (round (/ place 4.7849694539512483387184177063009674011993525936328803960997713980843374791106177275940814712824077009))))


;Decent
(define (expected-linear place)
  (+ (* 0.2073414905 place) 0.3862068966))


;Terrible
(define (expected-quadratic place)
  (+ (* 0.00071508025 (* place place)) (* 0.1851740029 place) .5044334975))

