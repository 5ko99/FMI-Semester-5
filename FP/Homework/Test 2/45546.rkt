#lang racket/base
;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 2
; 2020-12-12
;
; Име: Петко Каменов
; ФН: 45546
; Специалност: Информатика
; Курс: Трети
; Административна група: Първа 
; Начален час на контролното: 8:00
;



(provide (all-defined-out)) ; Запазете този ред, за да може всички функции,
                            ; които сте написали, да се експортират и да може да се тестват.


; Това е примерна функция. Можете да напишете решението си на нейното място.
(define (sample-function x) x)

(define head car)
(define tail cdr)

(define empty-tree '())
(define (empty-tree? t) (eq? t '()))
(define (node-tree tree) (car tree))
(define (left-tree tree) (cadr tree))
(define (right-tree tree) (caddr tree))
(define (is-leaf? t) (and (not (empty-tree? t)) (empty-tree? (left-tree t)) (empty-tree? (right-tree t))))
 
(define (tree-with-one-suc tree)
  (cond [(or (empty-tree? tree) (is-leaf? tree)) 0]
        [(or (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))) (+ 1 (tree-with-one-suc (left-tree tree)) (tree-with-one-suc (right-tree tree)))]
        [else (+ (tree-with-one-suc (left-tree tree)) (tree-with-one-suc (right-tree tree)))]
        ))

(define (tree-with-two-suc tree)
  (cond [(or (empty-tree? tree) (is-leaf? tree)) 0]
        [(and (not (empty-tree? (left-tree tree))) (not (empty-tree? (right-tree tree)))) (+ 1 (tree-with-two-suc (left-tree tree)) (tree-with-two-suc (right-tree tree)))]
        [else (+ (tree-with-two-suc (left-tree tree)) (tree-with-two-suc (right-tree tree)))]
        ))

(define (degenerate? tree)
  (if (empty-tree? tree) #t
      (let [(one-suc (tree-with-one-suc tree))
            (two-suc (tree-with-two-suc tree))]
        (if (> one-suc two-suc) #t #f))))

;2zad
(define (cover pts)
  (if (eq? pts '()) #f
      (let [(max-x (foldr max (head (head pts)) (map head pts)))
            (min-x (foldr min (head (head pts)) (map head pts)))
            (max-y (foldr max (tail (head pts)) (map tail pts)))
            (min-y (foldr min (tail (head pts)) (map tail pts)))]
        (cons (cons min-x max-y) (cons max-x min-y))
        )))
