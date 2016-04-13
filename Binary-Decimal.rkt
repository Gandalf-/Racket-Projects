#lang racket

(provide binary->decimal)
(provide decimal->binary)
(provide padding)

(define padding 8)  ; number of bits in a character

; Converts a binary number represented as a string to it's decimal equivalent
;
; string -> number
(define (binary->decimal input)
  (let convert ((list-chars (reverse
                              (string->list input)))
                (list-nums '() ))
    (if (not (empty? list-chars))
      (convert (cdr list-chars)
               (cons list-nums 
                     (ceiling (- 1 (/ 48 (char->integer (car list-chars)))))) )
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


; Converts a decimal number to its binary equivalent represented in 
; a string of 1's and 0's
;
; number -> string
(define (decimal->binary input)

  ;Find the largest two's power in the given input
  ;
  ; number -> number
  (define (find-highest-place input)
    (let loop ((current 1)
               (previous 0))
      (if (< input current)
        previous
        (loop (* 2 current) current))))

  ;Find all the factors of powers of two in the input
  ;
  ; number -> list of number
  (define (find-places input)
    (let loop ((output '())
               (current-place input))
      (if (= current-place 0)
        (reverse output)
        (loop (flatten (cons output (find-highest-place current-place)))
              (- current-place (find-highest-place current-place)))
        )))

  ;Add padding
  ;
  ; list of number -> list of number
  (define (add-pad in)
    (append
      (build-list (- padding (length in))
                  (lambda (x) 0))
      in))

  ; Turn a list of 1's and 0's into a string of '1's and '0's
  ;
  ; list of number -> string
  (define (numlist->string in)
    (list->string
      (map
        (lambda (x) 
          (if (= x 1) 
            #\1 
            #\0))
        (add-pad (reverse in)))))

  ; Check each power of two for a corresponding value in the places list,
  ; does the actual work here. Utilizes the above helper functions
  ;
  ; number -> string
  (define (make-output input)
    (let loop ((current-place 1)
               (input-list input)
               (output '() ))

      (if (empty? input-list)
        (numlist->string output)

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
    (make-output (find-places input))))


(define (test)
  (displayln (binary->decimal "00000010"))
  (displayln (decimal->binary 2))
  (displayln (decimal->binary 23498723498723))
  (define input "Hello there. What's going on? Congratulations on decoding this!"))
