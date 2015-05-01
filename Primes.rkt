#lang racket

(provide primes)

(define (primes limit)
  (let loop ((in (cddr
                   (build-list limit values)))
             (out '(1) ))
    (if (empty? in)
      (reverse out)
      (loop (filter 
              (lambda (x)
                (not
                  (= 0
                     (modulo x (car in)))))
              (cdr in))
            (cons (car in)
                  out))) ))

(primes 1000000)
