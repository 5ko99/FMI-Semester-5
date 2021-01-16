#lang racket
(define lst '(1 2 3 4 5 6))

(define (f1 item result)
  (if (even? item) (+ item result) result))

(define (f2 result item)
  (if (even? item) (+ item result) result))



(define (f)
  (display "*")
  5)

(define p (delay (f)))

(force p)
(force p)
  

   