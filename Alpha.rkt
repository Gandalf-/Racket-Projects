#lang racket

(define MAX-STARS 35)
(define ITERS 30)     ; number of lines to print
(define ALPHA 0.10)   ; higher alpha means the previous line has higher weight

; Weighting function
(define (weight-function old new)
  (let ((alpha ALPHA))
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
  (let ((max MAX-STARS)
        (iters ITERS))
    ;loops over the lines
    (let line-loop ((i 0)
               (out '())
               (prev (random max)))
      (if (= i iters)
          out
          (letrec 
            ; calculate the number of stars to add on this line
            ((base (weight-function prev (random max)))
             (new
               ;loops over the individual stars for this line
               (let star-line-loop ((i2 0)
                                    (out2 '() ))
                 (if (> i2 base)
                   ;Fill in remaining spaces with "space"
                   (flatten 
                     (cons (make-list (- max (length out2)) " ")
                           out2))
                   ; add the new star 
                   (star-line-loop (+ i2 1)
                                   (cons "*" out2))))))
            (line-loop (+ i 1)
                       ; add the new line of stars to the output
                       (cons new out)
                       ; the length of the new line is used as the weighting
                       ; for the next iteration
                       (length new)))))
    ))

; Print vertically
(define (vertical-print in)
  (define (vert-print in i iters max)
    (let line-loop ((in in)
                    (i i)
                    (iter 0)
                    (str ""))
      (if (= iter iters)
        (begin
          (print str)(newline)
          (when (< i (- max 1))
            (vert-print in (+ i 1) iters max)))

        (line-loop in
                   i
                   (+ iter 1)
                   (string-append
                     str
                     (list-ref 
                       (list-ref in iter) i)))) ))
  (vert-print in 0 ITERS MAX-STARS)
  (newline))

(vertical-print q)

;Print horizontally
(define d
  (map
    (lambda (x)
      (let print-line ((y x)
                       (str ""))
        (if (empty? y)
          (print str)
          (print-line (cdr y)
                      (string-append (car y) str))))
      (newline)
      x)
    q))
