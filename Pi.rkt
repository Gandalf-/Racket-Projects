#lang racket

(require "DividePrecise.rkt")

;PI CALCULATIONS
(define (pi-one k)
  (expt 16 (* -1 k)))

(define (pi-two k)
  (- (/ 4 (+ (* 8 k) 1))
     (/ 2 (+ (* 8 k) 4)) 
     (/ 1 (+ (* 8 k) 5))
     (/ 1 (+ (* 8 k) 6))))
        
(define (pi-find k)
  (* (pi-one k) (pi-two k)))

(define (pi-loop max)
  (let loop ((i 0) (sum 0))
    (if (= max i)
        sum
        (loop (+ i 1) (+ sum (pi-find i))))))

(define (make-pi k precision)
  (let* ((s (- (pi-loop k) 3))
         (d (denominator s))
         (n (numerator s)))
    (string-append "3." (substring (divide-precise n d precision) 2))))

;(make-pi 2000 100)    