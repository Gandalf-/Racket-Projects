#lang racket

(require racket/gui)

;GUI INIT
;---------------------------------------------
(define frame-width 600)
(define frame-height 400)
(define move-distance 20)
(define dot-size 20)

;Frame
(define frame
  (new frame%
       (label "Wandering Worm")
       (min-width frame-width)
       (min-height frame-height)))

(send frame create-status-line)
(send frame show #t)

;Panel
(define h-panel
  (new horizontal-panel%
       (parent frame)
       (stretchable-height #f)
       (style '(border))
       (border 2) ))

;Choose number of segments in worm body
(define num-segments 20)

;Allow changes to the number of segments in real time
(define segment-slider
  (new slider%
       (parent h-panel)
       (label "Length")
       (min-value 1)
       (max-value 40)
       (init-value 10)

       ;Callback means that this function is called whenever segment-slider's value changes
       (callback 
        (lambda (button event) 
          (set! num-segments (send segment-slider get-value))

          ;Check for crash values
          (when (and (> num-segments 20)
                     (= (send speed-slider get-value) 1))
            (send speed-slider set-value (+ (send speed-slider get-value) 1)))

          ;Remake the body
          (fill-body)
          (send canvas refresh)))))

;Choose animation speed
(define speed-slider
  (new slider%
       (parent h-panel)
       (label "Speed")
       (min-value 1)
       (max-value 10)
       (init-value 4)

       (callback 
         (lambda (button event)

           ;Check for crash values
           (when (and (> num-segments 20)
                      (= (send speed-slider get-value) 1))
             (send speed-slider set-value (+ (send speed-slider get-value) 1)))))))

;Choose head's new position angle
(define spread-slider
  (new slider%
       (parent h-panel)
       (label "Angle")
       (min-value 0)
       (max-value 30)
       (init-value 3)))

;DRAW
;--------------------------------------------
;Initialize body, random head pos, all others copy head
(define body '() )
(define init-done #f)

;Fills the body list with a number of random points
;Different calls are made depending on whether it's the initial set up, or 
; a later change the number of segments
(define (fill-body)
  (set! body '())

  ;The first call can't access canvas, so use default frame values
  (if (equal? #f init-done)
    (begin
      (let loop ((i 0))
        (unless (= i num-segments)
          (let ((head (make-object
                        point%
                        (random frame-width)
                        (random frame-height))))
            (set! body (append body (list head))))

          (loop (+ i 1))))
      (set! init-done #t))

    ;This is called when the init has been finished and the user changes
    ; the size of the window. The random starting postions of the dots
    ; now take up the 
    (let loop ((i 0))
      (unless (= i num-segments)
        (let ((head (make-object
                      point% 
                      (random (send canvas get-width))
                      (random (send canvas get-height)))))
          (set! body (append body (list head))))

        (loop (+ i 1)))) ))

(fill-body)

;Colors
(define color-green (make-object color% 14 130 3))
(define color-orange (make-object color% 206 102 11))

;Brush 
(define brush-orange (make-object brush% color-orange))

;Instructions object
;----------------------------------------------------------
;Contains (my-draw) and (move)
; my-draw draws each element of the body list
; move updates all the elements of the body list
(define previous-angle 1)

(define instructions%
  (class object%
         (public move my-draw)

         ;Draw body- draw an ellipse at the point given by each element
         ; of the body list
         (define (my-draw dc)
           (send dc set-brush brush-orange)

           (for-each 
             (lambda (point)
               (send dc draw-ellipse 
                     (send point get-x)
                     (send point get-y)
                     dot-size
                     dot-size))
             body))

         ;Move body
         ; Move all but the first element to the position of the element before it
         ; Choose a new position for the first element, and move it there
         (define (move)
           (define (find-new-x angle)
             (* (cos angle) 
                move-distance))

           (define (find-new-y angle)
             (* (sin angle)
                move-distance))

           (define (random-sign)
             (if (>= .5 (random))
               1
               -1))

           ;worker function that recursively goes through the body list
           (define (move-worker input)
             (unless (empty? input)
               (if (equal? (car input)
                           (car body))

                 ;Head case, choose new postion and move head there
                 (begin                 
                   (let ((angle (+ previous-angle 
                                   (* (random-sign)
                                      (+ (/ (send spread-slider get-value)
                                            10)
                                         (random 1))))))
                     ;Assign new x coord
                     (send (car input)
                           set-x (modulo (round (+ (send (car input) get-x)
                                                   (find-new-x angle)))
                                         (send canvas get-width)))
                     ;Assign new y coord
                     (send (car input)
                           set-y (modulo (round (+ (send (car input) get-y)
                                                   (find-new-y angle)))
                                         (send canvas get-height)))

                     (set! previous-angle angle)))

                 ;Body case, set everything to next in reversed list
                 (begin
                   (send (car input)
                         set-x (send (car (cdr input)) get-x))
                   (send (car input)
                         set-y (send (car (cdr input)) get-y))
                   (move-worker (cdr input))))))

           (move-worker (reverse body)))

         (super-new)))

;Create instructions object
(define instructions (new instructions%))

;RUN BOX, CANVASES
;---------------------------------------------
;Checkbox
(define choice
  (new check-box%
       (label "Run")
       (parent h-panel)

       ; Callback is called whenever the state of the check-box changes
       (callback
         (lambda (checkbox event)
           (if (send checkbox get-value)
             (thread-resume animate)
             (thread-suspend animate))))))

;Canvases
(define my-canvas%
  (class canvas%
         (override on-paint)

         ;Clear canvas then redraw using (my-draw)
         (define (on-paint)
           (let ((dc (send this get-dc)))
             (send dc clear)
             (send instructions my-draw dc)))

         (super-new)
         (send (send this get-dc) set-background
               color-green)))

(define canvas
  (new my-canvas%
       (parent frame)
       (style '(border))))

;THREAD
;---------------------------------------------
(define wait-time (/ 2 50))

(define animate
  (thread
    (lambda ()
      (let loop ()
        (send instructions move)
        (send canvas refresh)
        (sleep (/ (send speed-slider get-value)
                  50))
        (loop)))))

(thread-suspend animate)
