#lang racket

(define master-list (build-list 10 (lambda (x) (+ x 1))))

(define (filter-list input-list x)
  (let ((new-list input-list))
    (let loop ((input input-list) (search x))
      (if (empty? input)
          new-list
          (if (= 0 (modulo (car input) search))
              (begin
                (set! new-list (remove (car input) new-list))
                (loop (cdr input) search))
              (loop (cdr input) search))))))

(define (find-prime input-list)
  (unless (empty? input-list)
    (let ((x (car input-list)))
      (display x) (display " ")
      (if (= x 1)
          (find-prime (cdr input-list))
          (begin
            (set! input-list (filter-list input-list x))
            (find-prime (cdr input-list)))))))

(find-prime master-list)

