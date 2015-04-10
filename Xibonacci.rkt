#lang racket

;(define (fibo x)(let l ((c 1)(p 0)(i 0)(x (- x 1)))(if (> i x)c(l (+ c p)c(+ i 1)x))))
(define(fibo x)(let l((c 1)(p 0)(i 0)(x(- x 1)))(if(> i x)c(l (+ c p)c(+ i 1)x))))

;(fibo 25)

(define (listonacci x)
  (let loop ((output '(1 0))
             (i 0))
    (if (> i x)
        (reverse output)
        (loop (cons (+ (car output) (cadr output)) output)
              (+ i 1))
        )))

;(listonacci 25)

(define (fibo-word first second x)
  (let loop ((curr first)
             (prev second)
             (i 0))
    (when (< i x)
      (display curr) (newline)
      (loop (string-append curr prev)
            curr
            (+ i 1))
      )))

;(fibo-word "A" "B" 10)

;Makes a starting list of x size, first element is a 1
(define (make-list x)
  (let loop ((output '(1))
             (i 2))
    (if (> i x)
        (reverse output)
        (loop (cons 0 output)
              (+ i 1))
        )))             

;Grabs returns the first x elements in a list
(define (get-x x input-list)
  (let loop ((output '())
             (curr-list input-list)
             (i 1))
    (if (> i x)
        (reverse output)
        (loop (cons (car curr-list) output)
              (cdr curr-list)
              (+ i 1))
        )))

;Sums all the contents of a list
(define (sum-list input-list)
  (let loop ((sum 0)
             (curr-list input-list))
    (if (empty? curr-list)
        sum
        (loop (+ sum (car curr-list))
              (cdr curr-list))
        )))

;(sum-list '(1 2 3 4 5 6 7))
;(get-x 4 '(1 2 3 4 5 6 7 8 9 0))
;(make-list 6)

(define (xibonacci x upto override)
  (let ((start (make-list x)))
    
    (when (and (list? override)
               (= x (length override)))
      (set! start override))
    
    (let loop ((output start)
               (i 0))
      (if (> i upto)
          (display (reverse output))
          (loop (cons (sum-list (get-x x output))
                      output)
                (+ i 1))
          ))))

;Fibonacci
(xibonacci 3 25 '(1 1 1))

