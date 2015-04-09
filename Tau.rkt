#lang racket

(define (f x1 x2 x3)
  (/ 4 (* x1 x2 x3)))

(define (make-pi iters)
  (let ((c 3)
        (xs '(2 3 4)))
    (let loop ((t c)
               (xl xs)
               (ix 0)
               (add #t))
      (if (> ix iters)
           t
          (if add
              (loop (+ t (f (car xl) (cadr xl) (caddr xl)))
                    (map (lambda (x)
                           (+ 2 x))
                         xl)
                    (+ ix 1)
                    #f)
              (loop (- t (f (car xl) (cadr xl) (caddr xl)))
                    (map (lambda (x)
                           (+ 2 x))
                         xl)
                    (+ ix 1)
                    #t)
              )))
    ))

(make-pi 5000)
