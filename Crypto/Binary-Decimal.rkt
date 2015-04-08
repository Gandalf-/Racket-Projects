#lang racket

(provide binary->decimal)
(provide decimal->binary)
(provide padding)

(define padding 8)

;BINARY->DECIMAL
;----------------------------------------------------------------
(define (binary->decimal input)
  
  ;Input handling
  (let convert ((list-chars (reverse (string->list input)))
                (list-nums '() ))
    (if (not (empty? list-chars))
        (convert (cdr list-chars)
                 (cons list-nums (ceiling (- 1 (/ 48 (char->integer (car list-chars)))))) )
        
        ;Summation
        (let sum-nums ((input (flatten list-nums))
                       (sum 0)
                       (twos 1))
          (if (not (empty? input))
              (sum-nums (cdr input)
                        (+ sum (* twos (car input)))
                        (* twos 2))
              sum))
        )))

;(binary->decimal "00000010")


;DECIMAL->BINARY
;----------------------------------------------------------------
(define (decimal->binary input)
  
  ;Find the largest two's power in the given input
  (define (find-highest-place input)
    (let loop ((current 1)
               (previous 0))
      (if (< input current)
          previous
          (loop (* 2 current) current))))
  
  ;Find all the factors of powers of two in the input
  (define (find-places input)
    (let loop ((output '())
               (current-place input))
      (if (= current-place 0)
          (reverse output)
          (loop (flatten (cons output (find-highest-place current-place)))
                (- current-place (find-highest-place current-place)))
          )))
  
  ;Add padding
  (define (add-pad input)
    (let loop ((in input))
      (if (>= (length in) padding)
          in
          (loop (flatten (cons in 0)))
          )))
  
  ;Turn a list of binary numbers into a string
  (define (make-string inputlis)
    (let loop ((in (reverse (add-pad inputlis)))
               (out ""))
      (if (empty? in)
          out
          (if (= (car in) 1)
              (loop (cdr in)
                    (string-append out "1"))
              (loop (cdr in)
                    (string-append out "0")))
          )))
  
  ;Check each power of two for a corresponding value in the places list
  (define (make-output input)
    (let loop ((current-place 1)
               (input-list input)
               (output '() ))
      
      (if (empty? input-list)       
          (make-string output)
          (if (= (car input-list) current-place)
              (loop (* 2 current-place)
                    (cdr input-list)
                    (flatten (cons output 1)))
              (loop (* 2 current-place)
                    input-list
                    (flatten (cons output 0)))
              ))))
  (if (= 0 input)
      (make-string padding #\0)
      (make-output (find-places input)))
  )

;(decimal->binary 2)

;(define bas64 "SGVsbG8gdGhlcmUuIFdoYXQncyBnb2luZyBvbj8gQ29uZ3JhdHVsYXRpb25zIG9uIGRlY29kaW5nIHRoaXMh");
;(define input "Hello there. What's going on? Congratulations on decoding this!")
;(define input "App")

; STRING->BINARY
;----------------------------------------------------------------
; Takes a string and returns the binary represenatation of each character,
; in sequence. This is returned as a string
(define (string->binary input)
  (let loop ((listchar (string->list input))
             (out ""))
    (if (empty? listchar)
        ; Check for padding error
        (if (= 0 (modulo (string-length out) padding))
            out
            (string-append "0" out))
        ; Keep building the string
        (loop (cdr listchar)
              (string-append out
                             (decimal->binary (char->integer (car listchar))) ))
        )))


; BINARY->STRING
;----------------------------------------------------------------
; Takes a binary character string, breaks it in padding chunks and
; decodes each chunk back to a character. Puts the chars together to 
; make the output string
(define (binary->string input)
  
  ;Check for padding error
  (unless (= 0 (modulo (string-length input) padding))
    (set! input (string-append "0" input)))
  
  (let loop ((listchar (string->list input))
             (curword "")
             (out '() ))
    (if (empty? listchar)
        (list->string (flatten (cons out (integer->char (binary->decimal curword)))))
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

;(letrec ((x (binary->decimal (string->binary input)))
;         (y (binary->decimal (string->binary (number->string x))))
;         )
;  x
;  )