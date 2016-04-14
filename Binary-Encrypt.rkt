#lang racket

(require "Binary-Decimal.rkt")
(provide string->binary)
(provide binary->string)

; Takes a string and returns the binary represenatation of each character,
; in sequence. This is returned as a string
;
; string -> string
; (string->binary "Hello") -> "0110111101101100011011000110010101001000"
(define (string->binary input)

  (unless (string? input)
    (error "string->binary: invalid arguments"))

  (foldl string-append ""
         (map 
           (lambda (x) 
             (decimal->binary (char->integer x)))
           (string->list input))))


; Given the binary representation of a string, convert back to the orginal string
;
; string -> string
; (binary->string "0110111101101100011011000110010101001000") -> "Hello"
(define (binary->string input)

  (unless (string? input)
    (error "binary->string: invalid arguments"))

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


(define (test)
  (letrec ((str1 "Hello there. What's going on? Congratulations on decoding this!")
           (str2 "Hello")
           (input str2))
    (displayln (string->binary input))
    (displayln (binary->string (string->binary input)))))

;(test)
