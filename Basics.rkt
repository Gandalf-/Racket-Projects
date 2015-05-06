#lang racket

; Variables and functions are created with define
(define name "Racket")
(displayln name)

; Functions are of the form name arg1 arg2 ...
(define (square x)
  (* x x))

; You can also use lambda
; (lambda (arg1 arg2 ...) (some function))
(define cube 
  (lambda (x)
    (* x x x)))

; Functions calls are always inside parens
(displayln (square 5))
(displayln (cube 5))

; Lists are the main data structure
(define colors '("Green" "Blue" "Yellow" "Red"))
(define nums '(1 2 3 4 5 6 7 8 9))

; There are many different ways to move through a list
; (for-each apply-this-function to-this-list)
(for-each
  displayln
  colors)

; map applies the procedure you give it to each element,
; and returns a list
(map
  square
  nums)

; Calls can be nested to create more interesting effects
(for-each
  displayln
  (map
    square
    nums))

; Note that anywhere a function is required, you can use an
; anonymous lambda function in it's place
(map
  (lambda (x)
    (+ x 1))
  nums)

; Local variables can be constructed with 'let'
; (let ((local1 value1)
;       (local2 value2)
;       ...
;       )
;       ; Something that uses these variables
;       (lambda (...) ...)
;       )
(define (magic x)
  (let ((nice-number 42)
        (bad-number 890))
    (displayln
      (+ x nice-number bad-number))))

(magic 50)
