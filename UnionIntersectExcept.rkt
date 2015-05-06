#lang racket

(define a '(1 5 31 6 7 82 3 44 7 2 3 6 8 5 3))
(define b '(34 23 2 6 7 67 54 5 7 8 56 4 54 5))

; In first or second
(define (union a b)
  (let loop ((x (append a b))
             (out '()))
    (if (empty? x)
        (sort out <)
        (loop (remove* (list (car x)) x)
              (cons (car x) out)))))

; In first and second
(define (intersect a b)
  (let loop ((x (sort a <))
             (y (sort b <))
             (out '()))
    (cond
      ((or (empty? x)
           (empty? y))
       (reverse out))

      ; In both lists
      ((= (car x) (car y))
       (loop (remove* (list (car x)) x)
             (remove* (list (car y)) y)
             (cons (car x) out)))

      ; Only in x
      ((< (car x) (car y))
       (loop (cdr x)
             y
             out))

      ; Only in y
      ((> (car x) (car y))
       (loop x
             (cdr y)
             out))
      )))

; In first but not second
(define (except a b)
  (let loop ((x a)
             (y b)
             (out '() ))
    (if (or (empty? x)
            (empty? y))
        (sort out <)

        (loop (remove* (list (car x)) x)
              y
              (if (boolean? (member (car x) y))
                  (cons (car x) out)
                  out))
        )))

(union a b)
(intersect a b)
(except a b)
