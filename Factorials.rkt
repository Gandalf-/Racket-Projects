#lang racket

; computes n * n -1 * n-2 * ... iteratively
;
; number -> number
(define (factorial-iterative x)
  (let loop ((i 1) (total x))
    (if (= (- x 1) i)
      total
      (loop (+ 1 i) (* total (- x i))))))


; computes n * n -1 * n-2 * ... recursively
;
; number -> number
(define (factorial-recursive x)
  (cond ((<= x 0) 1)
        ((= x 1) 1)
        (else
          (* x (factorial-recursive (- x 1))))))


(define (test)
  (displayln (factorial-iterative 5))
  (displayln (factorial-recursive 5)))

(test)
