#lang racket

(require racket/gui)

;GUI INIT
;---------------------------------------------
(define frame-width 600)
(define frame-height 400)

;Frame
(define frame
  (new frame%
       (label "Draw Tree")
       (min-width frame-width)
       (min-height (+ 20 frame-height))))

(send frame create-status-line)
(send frame show #t)

;Panel for choices bar
(define top-panel
  (new horizontal-panel%
       (parent frame)
       (stretchable-height #f)))

(define bottom-panel
  (new horizontal-panel%
       (parent frame)
       (stretchable-height #f)
       (style '(border))
       (border 0)))

;Sliders
;-----------------------------------------------

;Definitions
(define y-change-min 10)
(define y-change-max 20)
(define maximum-depth 25)

;Height Change
(define height-slider
  (new slider%
       (label "Height")
       (parent top-panel)
       (min-value y-change-min)
       (max-value y-change-max)
       (init-value 15)))

;Branch spread
(define branch-min 5)
(define branch-max 25)

(define spread-slider
  (new slider%
       (label "Spread")
       (parent top-panel)
       (min-value branch-min)
       (max-value branch-max)
       (init-value 10)))

;Fancy color
(define color-button
  (new check-box%
       (parent top-panel)
       (label "Color")))

;Branch chance
(define branch-slider
  (new slider%
       (label "Branch")
       (parent bottom-panel)
       (min-value 0)
       (max-value 100)
       (init-value 60)))

;Decay chance
(define decay-slider
  (new slider%
       (label "Decay")
       (parent bottom-panel)
       (min-value 0)
       (max-value 10)
       (init-value 5)))

;Refresh button
(define refresh-button
  (new button%
       (parent bottom-panel)
       (label "Regenerate")
       (callback (lambda (b e) (send canvas refresh)))))

;Draw tree
;-----------------------------------------
(define continue-decay 5)   ; the % increase in chance for a branch to die off

(define root
  (make-object point% (/ frame-width 2) frame-height))

(define color-brown (make-object color% 125 65 0))
(define color-green (make-object color% 0 50 0))
(define color-black (make-object color% 0 0 0))

(define brown-pen (make-object pen% color-brown))
(define green-pen (make-object pen% color-green))
(define black-pen (make-object pen% color-black))

(define my-draw
  (lambda (canvas dc)

    (define (make-tree parent depth max-depth continue-chance)

      ;Done?
      (unless (= depth max-depth)
        ;Continue?
        (when (>= (/ continue-chance 100) (random))

          ;Colors? (Green for top, brown for bottom, black is default)
          (if (send color-button get-value)
            (if (>= depth (/ max-depth 4))
              (send dc set-pen green-pen)
              (send dc set-pen brown-pen))

            (send dc set-pen black-pen))

          ;are we making a branch or stem?
          (if (>= (/ (send branch-slider get-value)
                     100)
                  (random))

            ;Branches
            (let ((left-child
                    (make-object
                      point% 
                      ;Random x change from slider
                      (- (send parent get-x)
                         (+ branch-min
                            (random (send spread-slider get-value))))

                      ;Random y change from slider
                      (- (send parent get-y)
                         (+ y-change-min
                            (random (send height-slider get-value))))))

                  (right-child
                    (make-object 
                      point%
                      ;Random x change from slider
                      (+ (send parent get-x)
                         (+ branch-min 
                            (random (send spread-slider get-value))))

                      ;Random y change from slider
                      (- (send parent get-y)
                         (+ y-change-min 
                            (random (send height-slider get-value)))))))

              ;Draw lines between children and parent
              (send dc draw-lines (list parent left-child))
              (send dc draw-lines (list parent right-child))

              ; recurse left
              (make-tree
                left-child        ; root is now the left child
                (+ depth 1)       ; depth has increased
                max-depth         ; increase chance to die
                (- continue-chance (send decay-slider get-value)))
              
              ; recurse right
              (make-tree
                right-child       ; root is now the right child
                (+ depth 1)       ; depth has increased
                max-depth         ; increase chance to die
                (- continue-chance (send decay-slider get-value))))

            ;Stem
            (let ((stem 
                    (make-object
                      point%
                      ;No x change
                      (send parent get-x)

                      ;Random y change from slider
                      (- (send parent get-y)
                         (+ y-change-min 
                            (random (send height-slider get-value)))))))

              ; Draw line between old stem segment and new one
              (send dc draw-lines (list parent stem))

              ; Recurse
              (make-tree 
                stem            ; root is now this stem
                (+ depth 1)     ; increase depth
                max-depth       ; max-depth doesn't change across recursive calls
                (- continue-chance (send decay-slider get-value)))) ; increase chance to die
            ))))

    ;Begin the recursion, this is called first
    (make-tree 
      root                ; starting at the root
      0                   ; 0 current depth
      maximum-depth       ; the max-depth
      100)))              ; 100% chance to continue

;Canvas
(define canvas
  (new canvas%
       (parent frame)
       (min-width frame-width)
       (min-height frame-height)
       (paint-callback my-draw)))
