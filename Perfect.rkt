#lang racket

;Returns the sum of all the values in a list
(define (sum-list x)
  (if (null? x)
    0
    (+ (car x)
       (sum-list (cdr x)))))

;Checker functions
(define (divisor? x i)
  (= (modulo x i) 0))

(define (perfect? x)
  (if (= x 0)
    #f
    (= (sum-list (get-divisors x)) x)))

;Make a list of the divisors for a number x
(define (get-divisors x)
  (let loop ((i 1) (divisors '()))
    (if (= i x)
      divisors
      (loop (+ i 1) 
            (if (divisor? x i)
              (append divisors (list i))
              divisors )))))

;Searches through a list of integers up to x and returns 
; a list of the perfect numbers in that list
(define (find-perfect x)
  (filter perfect? (build-list x values)))

;Testing
;(find-perfect 1000)

(define (decimal->binary x)
  (string->number 
    (number->string x 2)))

;(decimal->binary 2305843008139952128)

(define (binary-to-decimal n)
  (define decimal-integer 0)
  (define dd 0)
  (define binary-string (number->string n))
  (define binary-list (string->list binary-string))
  (define power (- (length binary-list) 1))
  (define (loop l)
    (cond
      ((null? l) decimal-integer)
      (else
        (set! dd (- (char->integer (car l)) 48))
        (when (= dd 1) (set! decimal-integer (+ decimal-integer (expt 2 power))))
        (set! power (sub1 power))
        (loop (cdr l)))))
  (loop binary-list))

(binary-to-decimal 1111111111111111111111111111111111000000000000000000000000000000000)
