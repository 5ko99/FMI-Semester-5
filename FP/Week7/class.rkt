#lang racket
(define head car)
(define tail cdr)

(define (sub-range i j lst)
  (drop (take lst j) i))

(define (sub-matrix i1 j1 i2 j2 m)
  (let [(the-rows (sub-range i1 i2 m))]
    (map (lambda (row) (sub-range j1 j2 row)) the-rows)))

(define (any? p? lst)
  (and (not (null? lst))
       (or (p? (head lst))
           (any? p? (tail lst)))))

(define (max-by-size ms)
   (foldr (lambda (el res)
            (if (> (length el) (length res)) el res))
          (head ms)
          (tail ms)))

(define (all? p? lst)
  (not (any? (lambda (x) (not (p? x))) lst)))

(define test '((1 2 3) (4 5 6)))
;(apply append `((1 2) (3 4))) -> (1 2 3 4) <- (foldr append '() '((1 2) (3 4)))

(define (all-up-lefts m)
  (define num-rows (length m))
  (define num-cols (length (head m)))
  (apply append
         (map (lambda (row) (map (lambda (col) (cons row col)) (range 0 num-cols)))
       (range 0 num-rows)))
  )

(define (all-indices m)
  (define num-rows (length m))
  (define num-cols (length (head m)))
  (define up-lefts (all-up-lefts m))
  (define (max-size idx)
    (min (- num-rows (head idx))
         (- num-cols (tail idx))))
  (define (bump idx n)
    (cons (+ (head idx) n)
          (+ (tail idx n))))
  (apply append (map (lambda (idx)
                       (map (lambda (len) (cons idx (bump idx)))
                            (range 1 (+ 1 (max-size idx)))))
                     up-lefts))
  )

(define (find-submatrix ps m)
  (define (is-Ok el) ; дали числото l удовл. някой от предикатите
    (any? (lambda (p) (p el)) ps))
  (define (is-Ok-mat m)
    (all? (lambda (row) (all? is-Ok row)) m))
  (define good-submatrices
    (filter is-Ok-mat (all-submatrices m)))
  (if (null? good-submatrices) #f (max-by-size good-submatrices))
  )

;(define (?-mat m1 m2)
 ; (cond [(> (length m1) (length m2)) m1]
  ;      [(>(length m2) (length m1)) m2]
   ;     [else (if