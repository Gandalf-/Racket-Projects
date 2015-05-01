#lang racket

(require racket/gui)

;GUI INIT
;---------------------------------------------
(define frame-width 600)
(define frame-height 400)

(define depth 25)

;Frame
(define frame (new frame%
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

;Height Change
(define y-change-min 10)
(define y-change-max 20)

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
(define continue-decay 5)

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
      
      ;Colors
      (if (send color-button get-value)
          (begin
            (send dc set-pen brown-pen)
            
            ;Make the top 3/4 green
            (when (>= depth (/ max-depth 4))
              (send dc set-pen green-pen)))
          
          (send dc set-pen black-pen))
      
      (unless (= depth max-depth)
        ;Continue?
        (when (>= (/ continue-chance 100) (random))
          ;Branches or stem?
          (if (>= (/ (send branch-slider get-value) 100) (random))
              ;Branches
              (begin
                (let ((left-child (make-object point% 
                                    ;Random x change from slider
                                    (- (send parent get-x) (+ branch-min (random (send spread-slider get-value))))
                                    ;Random y change from slider
                                    (- (send parent get-y) (+ y-change-min
                                                              (random (send height-slider get-value))))))
                      (right-child (make-object point%
                                     ;Random x change from slider
                                     (+ (send parent get-x) (+ branch-min (random (send spread-slider get-value))))
                                     ;Random y change from slider
                                     (- (send parent get-y) (+ y-change-min 
                                                               (random (send height-slider get-value)))))))
                  ;Draw lines between children and parent
                  (send dc draw-lines (list parent left-child))
                  (send dc draw-lines (list parent right-child))
                  
                  (make-tree left-child (+ depth 1) max-depth (- continue-chance (send decay-slider get-value)))
                  (make-tree right-child (+ depth 1) max-depth (- continue-chance (send decay-slider get-value)))))
              ;Stem
              (begin
                (let ((stem (make-object point%
                              ;No x change
                              (send parent get-x)
                              ;Random y change from slider
                              (- (send parent get-y) (+ y-change-min 
                                                        (random (send height-slider get-value)))))))
                  
                  (send dc draw-lines (list parent stem))
                  
                  (make-tree stem (+ depth 1) max-depth (- continue-chance (send decay-slider get-value)))))
              ))))
    
    (make-tree root 0 depth 100)))


;Canvas
(define canvas
  (new canvas%
       (parent frame)
       (min-width frame-width)
       (min-height frame-height)
       (paint-callback my-draw)
       ))
