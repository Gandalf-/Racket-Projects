#lang racket

(define input "Hello there! Good luck decoding this!")
(define output "48656c6c6f20746865726521")

;Turns a string into a concatenated string of
; each character's integer as hex representation
; "Hello there!" -> "48656c6c6f20746865726521"
(define (string->hexstring x)
  (foldl string-append ""
         (reverse
          (map
           (lambda (n)
             (number->string
              (char->integer n) 16))
           (string->list x)))))

(string->hexstring input)

; Reverses what string->hexstring does
; "48656c6c6f20746865726521" -> "Hello there!"
(define (hexstring->string x)
  (define (clump w n)
    (let l ((in w)
            (out '() ))
      (if (empty? in)
          (reverse out)
          (l (cddr in)
             (cons (list->string (take in n))
                   out)))))
  (list->string
   (map
    (lambda (n)
      (integer->char
       (string->number n 16)))
    (clump (string->list x) 2))))

(hexstring->string (string->hexstring input))

;Is the character a number?
(define (num? y)
  (let ((i (char->integer y)))
    (and (< i 58)(> i 47))))

(define (char? y)
  (let ((i (char->integer y)))
    (or (> i 58)(< i 47))))

; Takes a string, and breaks into string chunks of
; contiguous members that meet proc, which is a bool
; num? : "656c6c203a29" -> "656" "6" "203" "29"
(define (break-with x proc)
  (let l ((lis (string->list x))
          (cur '() )
          (out '() ))
    (if (empty? lis)
        (if (empty? cur)
            (map (lambda (n) (list->string n))
                 (reverse out))
            
            (map (lambda (n) (list->string n))
                 (reverse (cons (reverse cur) out))))
        
        (if (proc (car lis))
            (l (cdr lis)
               (cons (car lis) cur)
               out)
            
            (l (cdr lis)
               '()
               (if (empty? cur)
                   out
                   (cons (reverse cur) out)))))
    ))

; Integer string to hex string
(define (numstring->hexstring x)
  (number->string
   (string->number x 10) 16))

; Breaks x into two pieces, those that meet rule1 and those that meet rule2. Then 
; applies proc1 to the first group, proc2 to the second group. Then weaves 
; the groups back together
(define (coalesce x rule1 rule2 proc1 proc2)
  (letrec ((a (break-with x rule1))     ; create list a by breaking x with rule1
           (b (break-with x rule2))     ; create list b by breaking x with rule2
           (f (rule1 (string-ref x 0))) ; determine whether A or B should be first in output
           
           (A (map (lambda (n) (proc1 n)) a)) ; apply proc1 to list a
           (B (map (lambda (n) (proc2 n)) b)) ; apply proc2 to list b
           
           (Alen (length A))
           (Blen (length B))
           (in (if f
                   (if (> Alen Blen)
                       (cons A (flatten (cons B "")))
                       (cons A B))
                   (if (> Blen Alen)
                       (cons B (flatten (cons A "")))
                       (cons B A)))))
    (let ((out
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
      (foldl string-append "" (reverse out))
      )))

(define (shrinkhex x)
  (coalesce x
            char?
            num?
            string-upcase
            numstring->hexstring))

(shrinkhex (string->hexstring "Hello there Wordpress"))

