#lang racket

(require "Binary-Decimal.rkt")
(provide string->binary)
(provide binary->string)

;(define input "Hello there. What's going on? Congratulations on decoding this!")

; Takes a string and returns the binary represenatation of each character,
; in sequence. This is returned as a string
(define (string->binary input)
  (foldl string-append ""
         (map 
           (lambda (x) 
             (decimal->binary (char->integer x)))
           (string->list input))))

;(string->binary input)

(define (binary->string input)
  (let loop ((in (string->list input))
             (out '() ))
    (if (empty? in)
      (list->string
        (reverse out))
      (loop (list-tail in padding)
            (append out
                    (list
                      (integer->char
                        (binary->decimal
                          (list->string (take in 8))))
                      )))
      )))

;(binary->string (string->binary input))
