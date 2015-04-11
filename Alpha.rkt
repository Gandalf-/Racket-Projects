#lang racket

(define MAX 35)
(define ITERS 55)

; Weighting function
(define (f old new)
  (let ((alpha 0.10))
    (+ (* old alpha)
       (* new (- 1 alpha))) ))

; Generate a list of random stars
; "**********           "
; "*******              "
; "*************        "
; "************         "
; "***************      "
; "********             "
(define q
  (let ((max MAX)
        (iters ITERS))
    ;Master loop
    (let loop ((i 0)
               (out '())
               (prev (random max)))
      (if (= i iters)
          out
          (letrec ((base
                    (f prev (random max)))
                   (new
                    ;Fill in stars
                    (let loop2 ((i2 0)
                                (out2 '() ))
                      (if (> i2 base)
                          ;Fill in remaining spaces with "space"
                          (flatten (cons (make-list (- max (length out2)) " ")
                                         out2))
                          
                          (loop2 (+ i2 1)
                                 (cons "*" out2))))))
            (loop (+ i 1)
                  (cons new
                        out)
                  (length new)))))
    ))

; Print vertically
(define (vertical-print in)
  (define (vert-print in i iters max)
    (let loop ((in in)
               (i i)
               (iter 0)
               (str ""))
      (if (= iter iters)
          (begin
            (print str)(newline)
            (when (< i (- max 1))
              (vert-print in (+ i 1) iters max)))
          
          (loop in
                i
                (+ iter 1)
                (string-append str
                               (list-ref 
                                (list-ref in iter) i)))) ))
  (vert-print in 0 ITERS MAX)
  (newline))

(vertical-print q)

;Print horizontally
(define d
  (map
   (λ (x)
     (let print-line ((y x)
                      (str ""))
       (if (empty? y)
           (print str)
           (print-line (cdr y)
                       (string-append (car y) str))))
     (newline)
     x)
   q))
