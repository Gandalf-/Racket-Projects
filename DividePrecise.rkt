#lang racket

(provide divide_precise)

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

(divide_precise 9 7)
