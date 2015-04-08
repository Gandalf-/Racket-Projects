#lang racket

(require "Binary-Decimal.rkt")
(provide string->binary)
(provide binary->string)

;(define input "Hello there. What's going on? Congratulations on decoding this!")
;(define input "Returns a new mutable string that is as long as the sum of the given strs' lengths,
; and that contains the concatenated characters of the given strs. If no strs are provided, the result is a zero-length string.")

; Takes a string and returns the binary represenatation of each character,
; in sequence. This is returned as a string
(define (string->binary input)
  (let loop ((listchar (string->list input))
             (out ""))
    (if (empty? listchar)
        (string-append out " ")
        (loop (cdr listchar)
              (string-append out
                             (decimal->binary (char->integer (car listchar))) ))
        )))
;(string->binary input)

(define (binary->string input)
  (let loop ((listchar (string->list input))
             (curword "")
             (out '() ))
    (if (empty? listchar)
        (list->string out)
        (if (= padding (string-length curword))
            ; A new letter has been finished
            (loop listchar
                  ""
                  (flatten (cons out (integer->char (binary->decimal curword)) )))
            ; Continue building curword
            (loop (cdr listchar)
                  (string-append curword (string (car listchar)))
                  out)
            ))
    ))

;(binary->string (string->binary input))




