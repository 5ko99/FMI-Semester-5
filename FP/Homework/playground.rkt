#lang racket

(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define (leaf? tree) (and (not (empty-tree? tree)) (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))))

(define head car)
(define tail cdr)

(define dlst '(1 2 3 4 5 6 4 7 5 56 46))

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

(define (maximum* cmp lst)
  (foldr (lambda (el rest)
           (if (cmp el rest)
               el
               rest
               ))
         (head lst)
         (tail lst)))

(define (bloom tree)
  (cond [(empty-tree? tree) empty-tree]
        [(leaf? tree) (let ((root (root-tree tree)))
                        (make-tree root (make-leaf root) (make-leaf root)))]
        [else (make-tree (root-tree tree) (bloom (left-tree tree)) (bloom (right-tree tree)))]
        ))

(define (prune tree)
  (cond [(or (empty-tree? tree) (leaf? tree)) empty-tree]
        [else (make-tree (root-tree tree) (prune (left-tree tree)) (prune (right-tree tree)))]
        ))

(define (max/min-in-tree f nv tree)
  (cond [(empty-tree? tree) nv]
        [(leaf? tree) (root-tree tree)]
        [else (f (root-tree tree) (max/min-in-tree f nv (left-tree tree)) (max/min-in-tree f nv (right-tree tree)))]
        ))

(define (avg tree)
  (cond [(empty-tree? tree) empty-tree]
        [(leaf? tree) tree]
        [else (let ((max (max/min-in-tree max -inf.0 tree)) (min (max/min-in-tree min +inf.0 tree)))
                (make-tree (/ (+ max min) 2) (avg (left-tree tree)) (avg (right-tree tree))))]
        ))

;all number at depth k from the root
(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(zero? k) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t)) (tree-level (- k 1) (right-tree t)))]
        ))

;6ta
(define (height t)
  (if (empty-tree? t) 0
      (+ 1 (max (height (left-tree t))
                (height (right-tree t))))))

(define (all-levels t)
  (map (lambda (i) (tree-level i t)) (range 0 (height t))))

;tree to list
(define (tree->list tree)
  (if (empty-tree? tree) empty-tree
      (append (tree->list (left-tree tree)) (list (root-tree tree)) (tree->list (right-tree tree)))
      ))

(define (bst-insert val tree)
  (cond [(empty-tree? tree) (make-leaf val)]
        [(<= val (root-tree tree)) (make-tree (root-tree tree) (bst-insert val (left-tree tree)) (right-tree tree))]
        [else (make-tree (root-tree tree) (left-tree tree) (bst-insert val (right-tree tree)))]
        ))

;sort
(define (tree-sort lst)
  (tree->list (foldr bst-insert empty-tree lst)))

;balanced tree?
(define (balanced? tree)
  (cond [(empty-tree? tree) #t]
        [(leaf? tree) #t]
        [else (and (<= (abs (- (height (left-tree tree)) (height (right-tree tree)))) 1) (balanced? (left-tree tree)) (balanced? (right-tree tree)))]
        ))
;ordered tree?
(define (ordered? tree)
  (cond [(empty-tree? tree) #t]
        [(leaf? tree) #t]
        [(empty-tree? (right-tree tree)) (and (>= (root-tree tree) (root-tree (left-tree tree))) (ordered? (left-tree tree)))]
        [(empty-tree? (left-tree tree)) (and (< (root-tree tree) (root-tree (right-tree tree))) (ordered? (right-tree tree)))]
        [else (and (>= (root-tree tree) (root-tree (left-tree tree))) (< (root-tree tree) (root-tree (right-tree tree)))
                   (ordered? (left-tree tree)) (ordered? (right-tree tree)))]
        ))


;Matricies
(define exm '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
(define mat '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))

(define (head-rows m) (head m))
(define (head-cols m) (map head m))
(define (tail-rows m) (tail m))
(define (tail-cols m) (map tail m))
(define (null-m? m) (or (null? m) (null? (head m))))

;1zad
(define (sub-range i j lst)
  (drop (take lst j) i))

(define (sub-matrix i1 j1 i2 j2 m)
  (let [(the-rows (sub-range i1 i2 m))]
    (map (lambda (row) (sub-range j1 j2 row)) the-rows)))

(define (foldr-matrix op-rows nv-rows op-elems nv-elems m)
  (foldr op-rows nv-rows
         (map (lambda (row) (foldr op-elems nv-elems row)) m)))

;uniques
(define (uniques lst)
  (if (null? lst) lst
      (let[(rest (uniques (tail lst)))]
        (if (member (head lst) rest) rest
            (cons (head lst) rest)))))

(define (group-by f lst)
  (define returned (uniques (map f lst)))
  (define (elements-for x)
    (filter (lambda (el) (equal? x (f el))) lst))
  (map (lambda (x)
         (list x (elements-for x)))
       returned))

;quicksort
(define (quicksort lst)
  (if (or (null? lst) (null? (tail lst)))
      lst
      (let [(pivot (head lst))
            (rest (tail lst))]
        (append (quicksort (filter (lambda (x) (< x pivot)) rest))
                (list pivot)
                (quicksort (filter (lambda (x) (>= x pivot)) rest))))))

;insertionsort
(define (insert val lst)
  (cond [(null? lst) (list val)]
        [(> val (head lst)) (cons (head lst) (insert val (tail lst)))]
        [else (cons val lst)]))

(define (insertion-sort lst)
  (foldr insert '() lst))
