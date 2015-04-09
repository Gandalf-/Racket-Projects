#lang racket

;f(x)'s
(define (xsquared x)
  (* x x))

(define (5x x)
  (* x 5))

(define (five x)
  5)

; How many increments to try when finding the max of f(x)
(define find-max-precision 100)

;Number of random points to check when finding the proportion
; above and below the function
(define prop-precision 100000)

;Master function
(define (integrate lb ub f)
  
  ;Find the highest y coordinate between the bounds
  ; of the function f
  (define (find-max-y lb ub f)
    (let loop ((max -inf.0)
               (x lb)
               (increment (/ (+ (abs ub) (abs lb)) find-max-precision))
               (fx (f lb)))
      
      (if (> x ub)
          max
          (if (> fx max)
              (loop fx
                    (+ x increment)
                    increment
                    (f (+ x increment)))
              (loop max
                    (+ x increment)
                    increment
                    (f (+ x increment)))
              )
          )))
  
  ;Globals
  (let ((max-y (find-max-y lb ub f)))
    (display "max-y ")(display max-y)(newline)
    
    ;Return a random point from x[lb - ub], y[0 - max-y]
    (define (rand-point lb ub max-y)
      (append (list (- (random (+ (abs lb) (abs ub))) (abs lb)))
              (list (random (round max-y))))
      )
    
    ;Returns #t if the point given is under the function f
    (define (under? randpt f)
      ;(if (y <= f(x) )
      (if (<= (floor (last randpt)) (ceiling (f (car randpt))))
          #t
          #f)
      )
    
    ;Gets the proportion of the total area that's under the graph
    (define (get-prop lb ub f)
      (let loop ((above 0)
                 (below 0)
                 ;(max-y (find-max-y lb ub f))
                 (i 0)
                 (total prop-precision))
        
        (if (> i total)
            ;Return result
            (begin
              ;(display above)(newline)
              ;(display below)(newline)
              (/ below (+ below above)))
            (if (under? 
                 (rand-point lb ub max-y)
                 f)
                ;Point was under the function
                (loop above
                      (+ 1 below)
                      ;max-y
                      (+ i 1)
                      total)
                ;Point was above the function
                (loop (+ 1 above)
                      below
                      ;max-y
                      (+ i 1)
                      total)
                ))
        ))
    (display "Prop: ")(display (get-prop lb ub f))(newline)
    
    ;Find the area under the function
    (define (get-result lb ub)
      (let ((total-area (+ (* max-y (abs ub))
                           (* max-y (abs lb)))))
        
        (display "Area: ")(display total-area)(newline)
        
        (* total-area (get-prop lb ub f))
        ))
    
    (get-result lb ub)
    ))

(integrate 0 10 5x)