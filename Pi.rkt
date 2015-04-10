#lang racket

;PRECISE DIVISION

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

;Prints x to a file
;------------------------------------------------
(define (print-this x name)
  (call-with-output-file name
    (lambda (output-port)
      (display x output-port))))

;PI CALCULATIONS
(define (pi-one k)
  (expt 16 (* -1 k)))

(define (pi-two k)
  (- (/ 4 (+ (* 8 k) 1))
     (/ 2 (+ (* 8 k) 4)) 
     (/ 1 (+ (* 8 k) 5))
     (/ 1 (+ (* 8 k) 6))))
        
(define (pi-find k)
  (* (pi-one k) (pi-two k)))


(define (pi-loop max)
  (let loop ((i 0) (sum 0))
    (if (= max i)
        sum
        (loop (+ i 1) (+ sum (pi-find i))))))

(print-this (pi-loop 1000) "Pi_Output.txt")
    