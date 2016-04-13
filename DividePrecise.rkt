#lang racket

(provide divide-precise)

; Divide (/ x y) with prec decimal places of accuracy
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
  
  (let loop ((out '() ) (i 0) (x x))
    (if (= i precision)
        (list-of-string->string out)
        (loop (if (= i 0)
                  (cons "." (cons (quot x y) out))
                  (cons (quot x y) out))
              (+ i 1)
              (* (remain x y) 10)))))

(divide-precise 10 12345 550)
