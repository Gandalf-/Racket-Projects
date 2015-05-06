#lang racket

(require racket/gui)
;(require "readword.rkt")

;DATA HANDLING
;---------------------------------------------
(define points '() )

(define (graph-from-file input)
  (call-with-input-file input
    (lambda (file)
      (let loop ((word (read-word file)))
        (when word
          (set! points (append points (list (string->number word))))
          (loop (read-word file)))))))

;TESTING
;---------------------------------------------
;(set! points '(1 1 2 3 5 8 13 21 34 55 89))
;(set! points '(1 2 5 6 4 8 9  1 3 4 5 6 7 8 9 4 1 3 4 5 6 3 4 5 9 7))
(set! points '(50 92 4 49 64 60 7 81 81 57 9 55 46 29 99 91 69 52 15 62 73 69 30 74 86 11 81 16 69 58))
;(set! points '(9 8 7 6 5 4 3 2 1 9 8 7 6 5 4 3 2 1 9 8 7 6 5 4 3 2 1 ))
;(set! points '(0 1 2 3 4 3 2 1 0 -1 -2 -3 -4 -3 -2 -1 0 1 2 3 4))

; Requires readword.rkt
;(graph-from-file "Primes/Differences.txt")

;GUI INIT
;---------------------------------------------
;Frame
(define frame 
  (new frame%
       (label "Graph Me! 1.2")
       (min-width 750)
       (min-height 550)))

(send frame create-status-line)
(send frame show #t)

;Panel for choices bar
(define h-panel
  (new horizontal-panel%
       (parent frame)
       (stretchable-height #f)
       (style '(border))
       (border 2)))

;Choices bar
(define drawing-choice
  (new choice%
       (parent h-panel)
       (label "Graph using: ")
       (choices '("Dots" "Lines"))
       (callback 
         (lambda (c e)
           (send canvas refresh)))))

(define graph-choice
  (new choice%
       (parent h-panel)
       (label "Type of graph: ")
       (choices '("Y values" "X,Y values"))
       (callback 
         (lambda (c e)
           (send canvas refresh)))))

;Determine largest element of points
(define (find-max input-list)
  (car
    (sort input-list >)))

(define y-largest (find-max points))
(define x-largest (length points))

;Draw graph
; Draw y coordinate scale
; Draw x coordinate scale
; Plot using lines or dots
(define my-draw
  (lambda (canvas dc)
    (let ((origin-offset 40))

      ;Draws y coordinate scale
      (define (draw-y)
        (let loop ((i 0))
          (unless (> i 10)
            (let ((y-value (number->string
                             (exact->inexact 
                               (* y-largest
                                  (/ i 10)))) ))
              (send dc draw-text
                    y-value
                    ;x
                    0
                    ;y
                    (- (send canvas get-height)
                       (* i 
                          (/ (send canvas get-height) 10))
                       origin-offset)))
            (loop (+ i 1)))))

      ;Draws x coordinate scale
      (define (draw-x)
        (let loop ((i 10))
          (unless (< i 0)
            ;Value to be printed
            (let ((x-value (number->string
                             (exact->inexact 
                               (- x-largest
                                  (* x-largest (/ i 10))))))) 
              (send dc draw-text
                    x-value
                    ;x
                    (- (send canvas get-width)
                       (* i 
                          (/ (send canvas get-width) 10))
                       origin-offset)
                    ;y
                    (- (send canvas get-height) 20)))
            (loop (- i 1)))
          ))

      ;Begin drawing graph
      (let loop ((i 0)
                 (current points)
                 (previous 0))
        (unless (= i (length points))
          (case (send drawing-choice get-selection)
            ((0)
             ; Draw using dots, plotting y values
             (send dc draw-ellipse
                   ;x
                   (+ origin-offset
                      (* i 
                         (/ (send canvas get-width)
                            (length points))))
                   ;y
                   (- (- (send canvas get-height)
                         (* (- (send canvas get-height)
                               origin-offset)
                            (/ (car current) y-largest)))
                      origin-offset)
                   5 5
                   ))

            ((1) ; Draw using lines, plotting y values
             (send dc draw-lines
                   (list
                     (make-object point%
                                  ;x
                                  (+ origin-offset 
                                     (* i (/ (send canvas get-width)
                                             (length points))))
                                  ;y
                                  (- (- (send canvas get-height)
                                        (* (- (send canvas get-height)
                                              origin-offset)
                                           (/ (car current) y-largest)))
                                     origin-offset))

                     (make-object point%
                                  ;x
                                  (+ origin-offset (* (- i 1) 
                                                      (/ (send canvas get-width)
                                                         (length points))))
                                  ;y
                                  (- (- (send canvas get-height)
                                        (* (- (send canvas get-height)
                                              origin-offset)
                                           (/ previous y-largest)))
                                     origin-offset))))
             (set! previous (car current))))
          (loop (+ i 1) (cdr current) previous)))

      (draw-y)
      (draw-x))) )

(define canvas
  (new canvas%
       (parent frame)
       (min-width 300)
       (min-height 300)
       (paint-callback my-draw)))
