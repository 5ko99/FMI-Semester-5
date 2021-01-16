#lang racket

(require racket/stream)

(define (cycle lst)

  (define (loop remaining)
    (if (null? remaining)
        (loop lst)
        (stream-cons (car remaining)
                     (loop (cdr remaining)))))

  (if (null? lst)
      empty-stream
      (loop lst))
)

(define (retrieve n str)
  (if (= n 0)
      (stream-first str)
      (retrieve (- n 1) (stream-rest str))))

(retrieve 4 (cycle '(1 2 3)))



(define p (delay (f a b)))

(define (f x y) (+ x y))
(define a 4)
(define b 2)

(force p)