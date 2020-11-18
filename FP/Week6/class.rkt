#lang racket
(define head car)
(define tail cdr)
;map,filter,foldr,any?,all?,drop,take,dropWhile,takeWhile
;list-ref

(define (dropWhile p? lst)
  (cond [(null? lst) '()]
        [(p? (head lst)) (dropWhile p? (tail lst))]
        [else lst]
        ))

;helper functions range(1 5)
(define (head-rows m) (head m))
(define (head-cols m) (map head m))
(define (tail-rows m) (tail m))
(define (tail-cols m) (map tail m))
(define (null-m? m) (or (null? m) (null? (head m))))

(define m '((1 2 3 4 5 6)
            (2 3 4 5 8 9)
            (0 2 6 4 8 2)
            (5 9 2 3 8 9)))

;1zad
(define (sub-range i j lst)
  (drop (take lst j) i))

(define (sub-matrix i1 j1 i2 j2 m)
  [let [(the-rows (sub-range i1 i2 m))]
    (map (lambda (row) (sub-range j1 j2 row)) the-rows)])

;2zad
(define (foldr-matrix op-rows nv-rows op-elems nv-elems m)
  (foldr op-rows nv-rows
         (map (lambda (row) (foldr op-elems nv-elems row)) m)))

;trees
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

;example-tree
(define test-tree
  (make-tree 10
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

;zad
(define (height t)
  (if (empty-tree? t) 0
      (+ 1 (max (height (left-tree t))
                (height (right-tree t))
                ))))

;4
(define (tree-sum t)
  (cond [(empty-tree? t) 0]
        [else (+ (root-tree t) (tree-sum (left-tree t)) (tree-sum (right-tree t)))]
        ))
    

;5
(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(zero? k) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]))

;6



(define (zipWith f lst1 lst2)
  (cond [(null? lst1) lst2]
        [(null? lst2) lst1]
        [else (cons (f (head lst1) (head lst2))
            (zipWith f (tail lst1) (tail lst2)))]
        ))
 
(define (all-levels* t)
  (if (empty-tree? t) '()
      (cons (list (root-tree t))
            (zipWith append
               (all-levels* (left-tree t))
               (all-levels* (right-tree t))))))

;7
(define (tree-map f t)
  (if (empty-tree? t) empty-tree
      (make-tree (f (root-tree t))
            (tree-map f (left-tree t))
            (tree-map f (right-tree t)))))