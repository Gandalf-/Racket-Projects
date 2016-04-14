#lang racket

; Turns a string into a concatenated string containing
; each character's ascii number in hexidecimal
;
; string -> string
; (string->hexstring "Hello there!") -> "48656c6c6f20746865726521"
(define (string->hexstring x)

  (unless (string? x)
    (error "string->hexstring: invalid arguments"))

  (foldl string-append ""
         (reverse
          (map
           (lambda (n)
             (number->string
              (char->integer n) 16))
           (string->list x)))))


; Reverses what string->hexstring does
;
; string -> string
; (hexstring->string "48656c6c6f20746865726521") -> "Hello there!"
(define (hexstring->string x)
  (define (clump w n)
    (let l ((in w)
            (out '() ))
      (if (empty? in)
          (reverse out)
          (l (cddr in)
             (cons (list->string (take in n))
                   out)))))

  (unless (string? x)
    (error "hexstring->string: invalid arguments"))

  (list->string
   (map
    (lambda (n)
      (integer->char
       (string->number n 16)))
    (clump (string->list x) 2))))


;Is the character a number?
;
; char -> boolean
; (num? '4') -> #true
(define (num? y)
  (let ((i (char->integer y)))
    (and (< i 58)(> i 47))))


;Is the character a number?
;
; char -> boolean
; (char? 'k') -> #true
(define (char? y)
  (let ((i (char->integer y)))
    (or (> i 58)(< i 47))))


; Takes a string, and breaks into string chunks of
; contiguous members that meet proc, which is a procedure that
; returns a boolean
;
; string, procedure (char -> boolean) -> list of strings
; (break-with num? "656c6c203a29") -> '("656" "6" "203" "29")
(define (break-with input proc)

  (unless (and (string? input) (procedure? proc))
    (error "break-with: invalid arguments"))

  (let loop ((input-list (string->list input))
             (current-group '() )
             (output '() ))

    (if (empty? input-list)
      ; done, check if we need to add the current group to output
      (if (empty? current-group)
        (map (lambda (e) (list->string e))
             (reverse output))

        (map (lambda (e) (list->string e))
             (reverse 
               (cons (reverse current-group)
                     output))))

      ; not done, either keep building the current group or add it and start over
      (if (proc (car input-list))
        (loop (cdr input-list)
              (cons (car input-list) 
                    current-group)
              output)

        (loop (cdr input-list)
              '()
              (if (empty? current-group)
                output
                (cons (reverse current-group) output)))))
    ))


; convert decimal string to a hexidecimal string
;
; string -> string
; (numstring->hexstring "2349823") -> "23daff"
(define (numstring->hexstring x)
  (number->string
    (string->number x 10) 16))


; Breaks input into two pieces, pieces that meet ruleA and those that meet ruleB.
; Then applies procA to the first group, procB to the second group. 
; Then weaves the groups back together
;
; string, procedure (char -> boolean), procedure (string -> string),
; procedure (char -> boolean), procedure (string -> string) -> string
;
; #see (shrinkhex) for example usage
(define (coalesce input ruleA procA ruleB procB)

  (unless (and (string? input) (procedure? ruleA) (procedure? procA)
               (procedure? ruleB) (procedure? procB))
    (error "coalesce: invalid arguments"))

  (letrec
    ; create groups according to rules
    ((groupA (break-with input ruleA)) 
     (groupB (break-with input ruleB))

     ; determine which group starts weave first
     (Afirst? (ruleA (string-ref input 0)))

     ; apply correct procedure to matching group
     (A (map
          (lambda (e) (procA e))
          groupA))
     (B (map
          (lambda (e) (procB e))
          groupB))

     ; get lengths once because we need them a couple times
     (lenA (length A))
     (lenB (length B))

     ; prepare for output, make sure we have consistent lengths for (map)
     (in (if Afirst?
           (if (> lenA lenB)
             (cons A (flatten (cons B "")))
             (cons A B))
           (if (> lenB lenA)
             (cons B (flatten (cons A "")))
             (cons B A))))

     ; build the output
     (out
       (flatten
         (map
           (lambda (i j)
             (cond
               ((= 0 (string-length i)) j)
               ((= 0 (string-length j)) i)
               (else 
                 (cons i j))))
           (car in)(cdr in)
           ))))

    ; append all the strings together
    (foldl string-append "" (reverse out))
    ))

; example usage for coalesce
;
; given a string, go through and group contiguous groups of letters and numbers
; for each group of letters, uppercase them
; for each group of numbers, convert them to hexidecimal
; weave the groups back together
;
; string -> string
(define (shrinkhex x)
  (coalesce x
            char?
            string-upcase
            num?
            numstring->hexstring))

(define (test)
  (let ((input "Hello there! Good luck decoding this!"))
    (displayln (string->hexstring input))
    (displayln (hexstring->string (string->hexstring input)))
    (displayln (numstring->hexstring "2349823"))
    (displayln (string->hexstring "Hello there Wordpress"))
    (displayln (shrinkhex (string->hexstring "Hello there Wordpress")))))

(test)
