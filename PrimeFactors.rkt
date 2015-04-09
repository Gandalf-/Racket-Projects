#lang racket

(define input "Hello there. What's going on? Congratulations on decoding this!")

(define (string->decimal input)
  (let loop ((in (string->list input))
             (out ""))
    (if (empty? in)
        out
        (loop (cdr in)
              (string-append out (number->string (char->integer (car in)))))
        )))

;(string->decimal input)

(define (prime-factors x)
  (let loop ((factors '(1) )
             (d 2)
             (n x))
    
    (when (> (* d d) n)
      (if (> n 1)
          (flatten (cons factors n))
          factors))
    
    (if (= n 1)
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

(prime-factors 1481131213)
(prime-factors 717413676)
(prime-factors 69724573273930333651)
(prime-factors 2061538749789532761)
(prime-factors 4204684594679497881)
(prime-factors 245737669504892523)
(prime-factors 10123872924213782016)
(prime-factors 457921847376287195)
(prime-factors 9239763944475816737)
