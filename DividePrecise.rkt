#lang racket

(require "readword.rkt" )

(provide divide_precise)

;WHOLE AND REMAINDER
;-------------------------------------------------------------
(define (divide_remainder x y)
  (define whole (floor (/ x y)))
  (display "Whole: ")(display whole)
  (newline)
  (display "Remainder: ")(display (- x (* whole y)))
  (newline)
  (newline))

;(divide_remainder 21 4)

;PRECISION
;-------------------------------------------------------------
;Define the results list and precision (number of decimals)
(define result '() )
(define precision 100)

;Prints the result list
(define (print_result input counter)
  (unless (empty? input)
    (if (= 1 counter)
        (begin
          (display ".")
          (print_result input (+ 1 counter)))
        (begin
          (display (car input))
          (print_result (cdr input) (+ 1 counter))))))

;Master for divide_precise_slave and print_results
(define (divide_precise x y)
  
  ;Makes calculations and saves to results
  (define (divide_precise_slave x y counter)
    (unless (> counter precision)
      (begin
        (define whole (floor (/ x y)))
        (define remainder (- x (* whole y)))
        (set! result (append result (list whole)))
        (divide_precise_slave (* 10 remainder) y (+ 1 counter)))))
  
  (divide_precise_slave x y 0)
  (print_result result 0))

;(divide_precise 9 7)

;Get number from file
(define (get-number input-file)
  (call-with-input-file input-file
    (lambda (file)
      (read-word file))))

;Divide two numbers in seperate files
(define (divide_from_file x y)
  (divide_precise (string->number (get-number x)) (string->number (get-number y))))
  
;(divide_from_file "Fibonacci/Data/two-thousand.txt"
;                  "Fibonacci/Data/two-thousand-minus.txt")

;(divide_precise 22 5)

 