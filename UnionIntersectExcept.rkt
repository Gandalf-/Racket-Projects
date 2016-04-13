#lang racket

(define a '(1 5 31 6 7 82 3 44 7 2 3 6 8 5 3))
(define b '(34 23 2 6 7 67 54 5 7 8 56 4 54 5))

; in first OR second
; --------------------
; append the lists together, then remove duplicates
;
; list of number, list of number -> list of number
(define (union a b)
  (let loop ((x (append a b))
             (out '()))
    (if (empty? x)
        (sort out <)
        (loop (remove* (list (car x)) x)
              (cons (car x) out)))))

; in first AND second
; --------------------
; sort, then use comparisons to determine membership quickly
;
; list of number, list of number -> list of number
(define (intersect a b)
  (let loop ((first (sort a <))   ; sort smallest to largest
             (second (sort b <))  ; sort smallest to largest
             (out '()))           ; output variable we'll build as we go along
    (cond
      ((or (empty? first)         ; finished, return output
           (empty? second))
       (reverse out))

      ; the element is in both lists, add it to output
      ((= (car first) (car second))
       (loop (remove* (list (car first)) first)
             (remove* (list (car second)) second)
             (cons (car first) out)))

      ; only in first
      ((< (car first) (car second))
       (loop (cdr first)
             second
             out))

      ; only in second
      ((> (car first) (car second))
       (loop first
             (cdr second)
             out))
      )))

; in first BUT NOT second
; --------------------
; attempt to remove every element in second from first
;
; list of number, list of number -> list of number
(define (except a b)
  (let loop ((first a)
             (second b)
             (out '() ))
    (if (or (empty? first)
            (empty? second))
        (sort out <)

        (loop (remove* (list (car first)) first)
              second
              (if (boolean? (member (car first) second))
                  (cons (car first) out)
                  out))
        )))

(define (test)
  (displayln (union a b))
  (displayln (intersect a b))
  (displayln (except a b)))

(test)
