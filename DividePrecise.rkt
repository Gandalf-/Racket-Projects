#lang racket

(provide divide-precise)

; Divide (/ x y) with 'precision' decimal places of accuracy
;
; number, number, number -> string
(define (divide-precise x y precision)
  
  (define (list-of-string->string x)
    (foldl string-append "" x))
  
  (define (floor-integer x)
    (cdr (member 
          #\. (reverse 
               (string->list 
                (number->string x))))))
  
  (define (quot x y)
    (list-of-string->string
     (map string
          (floor-integer
           (exact->inexact (/ x y))))))
  
  (define (remain x y)
    (- x (* (string->number (quot x y)) y)))

  (define (str-abs x)
    (number->string
      (abs (string->number x))))

  (unless (and (number? x) (number? y)
               (exact-nonnegative-integer? precision))
    (error "divide-precise: invalid arguments"))

  (let loop ((out '() ) (i 0) (x x))
    (if (= i precision)
      (list-of-string->string out)

      (loop (if (= i 0)
              (cons "." (cons (quot x y) out))
              (cons (str-abs (quot x y)) out))
            (+ i 1)
            (* (remain x y) 10)))))

(define (test)
  (displayln (divide-precise 10 12345 550))
  (displayln (divide-precise 22.2349 -7.123 1000))
  (displayln (divide-precise -20 -4 100))
  (displayln (divide-precise -20 1 -100)))

(test)
