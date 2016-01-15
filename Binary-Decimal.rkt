#lang racket

(provide binary->decimal)
(provide decimal->binary)
(provide padding)

(define padding 8)  ; number of bits in a character

;BINARY->DECIMAL
; string of 1's and 0's -> positive integer
(define (binary->decimal input)
  (let convert ((list-chars (reverse
                              (string->list input)))
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
        (loop (flatten (cons output (find-highest-place1 current-place)))
              (- current-place (find-highest-place1 current-place)))
        )))

  ;Add padding
  (define (add-pad in)
    (append
      (build-list (- padding (length in))
                  (lambda (x) 0))
      in))

  ;Turn a list of binary numbers into a string
  (define (numlist->string in)
    (list->string
      (map
        (lambda (x) (if (= x 1) #\1 #\0))
        (add-pad (reverse in)))))

  ;Check each power of two for a corresponding value in the places list
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
    (make-output (find-places input)))
  )

;(decimal->binary 2)
;(decimal->binary 23498723498723)

;(define bas64 "SGVsbG8gdGhlcmUuIFdoYXQncyBnb2luZyBvbj8gQ29uZ3JhdHVsYXRpb25zIG9uIGRlY29kaW5nIHRoaXMh");
;(define input "Hello there. What's going on? Congratulations on decoding this!")
;(define input "App")
