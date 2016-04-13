#lang racket

(provide primes)

; Returns all a list of all prime numbers less than 'limit' 
; using the Sieve of Eratosthenes. What we do is build a list of all
; numbers up to the limit. Then we go through the list and do the following:
;
; while the input list is not empty:
;   let x be the current number
;   add x to the output list
;   remove all other numbers in the input list that are divisible by x
;   remove x from the input list
;
; number -> number
(define (primes limit)
  (let loop 
    ; build a list of all numbers up to the limit
    ((input (cddr (build-list limit values)))
     ; initialize our output list
     (output '(1) ))

    (if (empty? input)
      ; building the list in reverse allows us to use 'cons', 
      ; which is much faster than append
      (reverse output)

      (loop 
        ; remove all other elements divisible by the current element
        (filter 
          (lambda (x) (not (= 0 (modulo x (car input)))))
          (cdr input))

        ; add the current element to the output
        (cons (car input)
              output))) ))

(primes 10000)
