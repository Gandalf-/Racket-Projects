#lang racket

(provide binary->decimal)
(provide decimal->binary)
(provide padding)

; number of bits in a character, makes numbers uniform width.
(define padding 8)  

; Converts a binary number represented as a string to it's decimal equivalent
;
; string -> number
; (binary->decimal "00000010") -> 2
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


;Find the largest two's power in the given input
;
; number -> number
; (find-highest-place 567) -> 512
(define (find-highest-place input)
  (let loop ((current 1)
             (previous 0))
    (if (< input current)
      previous
      (loop (* 2 current) current))))

;Find all the factors of powers of two in the input
;
; number -> list of number
; (find-places 567) -> (1 2 4 16 32 512)
(define (find-places input)
  (let loop ((output '())
             (current-place input))
    (if (= current-place 0)
      (reverse output)
      (loop (flatten (cons output (find-highest-place current-place)))
            (- current-place (find-highest-place current-place)))
      )))

; Add padding a binary number. Padding amount declared globally
;
; list of number -> list of number
; (add-pad '(1 0)) -> '(0 0 0 0 0 0 1 0)
(define (add-pad in)
  (if (> (length in) padding)
    in
    (append
      (build-list (- padding (length in))
                  (lambda (x) 0))
      in)))

; Turn a list of 1's and 0's into a string of '1's and '0's
;
; list of number -> string
; (numlist->string '(0 0 0 0 0 0 1 0)) -> "00000010"
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
; (make-output '(1 2 16 32)) -> "00110011"
(define (make-output input)
  (let loop ((current-place 1)
             (input-list input)
             (output '() ))

    (if (empty? input-list)
      (numlist->string output)

      (if (= (car input-list) current-place)
        ; input has this power of 2, add a one
        (loop (* 2 current-place)
              (cdr input-list)
              (flatten (cons output 1)))

        ; input doesn't have this power of 2, add a zero
        (loop (* 2 current-place)
              input-list
              (flatten (cons output 0)))
        ))))

; Converts a decimal number to its binary equivalent represented in 
; a string of 1's and 0's
;
; number -> string
; (decimal->binary 2) -> "00000010"
(define (decimal->binary input)
  (if (= 0 input)
    (make-string padding #\0)
    (make-output (find-places input))))


(define (test)
  (displayln (binary->decimal "00000010"))
  (displayln (find-highest-place 567))
  (displayln (find-places 567))
  (displayln (add-pad '(1 0)))
  (displayln (numlist->string '(0 0 0 0 0 0 1 0)))
  (displayln (make-output '(1 2 16 32)))
  (displayln (decimal->binary 2))
  (displayln (decimal->binary 23498723498723)))

;(test)
