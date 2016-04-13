#lang racket

; computes the prime factors of x
;
; number -> number
(define (prime-factors x)
  (let loop ((factors '(1) )
             (d 2)
             (n x))
    
    (when (> (* d d) n)
      (if (> n 1)
          (flatten (cons factors n))
          factors))
    
    (if (<= n 1)
        (flatten (cons factors x))
        (if (= 0 (modulo n d))
            (loop (flatten (cons factors d))
                  (+ 1 d)
                  (/ n d))
            (loop factors
                  (+ 1 d)
                  n)
            ))
    ))

(define (test)
  (displayln (prime-factors 1481131213))
  (displayln (prime-factors 71741)))

(test)
