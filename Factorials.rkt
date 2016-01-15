#lang racket

; computes n ^ n
(define (exponential-factorial x)
  (let loop ((i 1) (total x))
    (if (= x i)
        total         ; true  -> return total
        (loop         ; false -> continue recursion
          (+ 1 i)           ; increment i
          (* total x)))))   ; add exponential factor

(exponential-factorial 5)

; computes n * n -1 * n-2 * ...
(define (factorial x)
  (let loop ((i 1) (total x))
    (if (= (- x 1) i)
      total
      (loop (+ 1 i) (* total (- x i))))))

(factorial 5)
