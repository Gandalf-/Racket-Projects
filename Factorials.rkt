#lang racket

(define (factorial x)
  (let loop ((i 0) (num x))
    (if (= x i)
        num
        (loop (+ 1 i) (* num x)))))

(factorial 5)

(define (factorial-falling x)
  (let loop ((i 1) (num x))
    (if (= (- x 1) i)
        num
        (loop (+ 1 i) (* num (- x i))))))

(factorial-falling 5)
