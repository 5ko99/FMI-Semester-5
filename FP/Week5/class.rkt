#lang racket
(define head car)
(define tail cdr)

;6
(define (uniques lst)
  (if [null? lst] '()
      (let [(rest (uniques (tail lst)))]
        (if (member (head lst) rest)
            rest
            (cons (head lst) (uniques (tail lst)))))))

(define (uniques* lst)
  (if [null? lst] '()
      (const (head lst) (uniques* (filter (lambda (x) (not (equal? x (head lst)))) (tail lst))
                                  ))))

(define (uniques** lst)
  (define (helper lst res)
    (cond [(null? lst) res]
          [(member (head lst) res) (helper (tail lst) res)]
          [else (helper (tail lst) (const (head lst) res))]
          ))
  (helper lst '()))

(define (foldr op nv lst)
  (if (null? lst) nv
      (op (head lst)
          (foldr op nv (tail lst)))))

(define (maximum lst)
  (foldr max (head lst) (tail lst)))

(define (uniques*** lst)
  (foldr (lambda (el res)
           (if (member el res) res
               (cons el res)))
         '() lst))

(define (length* lst)
  (foldr (lambda (el res) (+ 1 res)) 0 lst))

(define (map* f lst)
  (foldr (lambda (el res) (cons (f el) res)) '() lst))

(define (filter* p? lst)
  (foldr (lambda (el res) (if (p? el) (cons el res) res)) '() lst))

;7 i 8
(define (insertion-sort lst)
  (foldr insert '() lst))

(define (insert el lst)
  (cond [(null? lst) (cons el '())]
        [(<= el (head lst)) (cons el lst)]
        [else (cons (head lst) (insert el (tail lst)))]
        ))

(define (quicksort lst)
  (if (or (null? lst) (null? (tail lst)))
      lst
      (let [(pivot (head lst))
            (rest (tail lst))]
      (append (quicksort (filter (lambda (x) (< x pivot)) rest))
              (list pivot)
              (quicksort (filter (lambda (x) (>= x pivot)) rest))))))

;10
(define (compose f g)
  (lambda (x) (f (g x))))

(define (compose-n . fns)
  (foldr compose (lambda (x) x) fns))

(define (1+ x) (+ x 1))
(define (sq x) (* x x))

;11
(define (group-by f lst)
  (define returned (uniques (map f lst)))
  (define (elements-for x)
    (filter (lambda (el) (equal? x (f el))) lst))
  (map (lambda (x)
         (list x (elements-for x)))
       returned))