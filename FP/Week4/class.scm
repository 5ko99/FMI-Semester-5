(define p (cons 5 "Petko"))
(car p);first
(cdr p);secod
(define q (cons 2 (cons 3 4)))
(car (cdr q))
(cadr q)
(cddr q)
;'()

(define (length1 lst)
  (if (null? lst)
      0
      (+ 1 (length (tail lst)))))

(define head car)
(define tail cdr)

(define (member? x lst)
  (cond ((null? lst) #f)
        ((equal? (head lst) x) #t)
        (else (member? x (tail lst)))))

(define (map1 f lst)
  (if (null? lst) '()
      (cons (f (head lst))
            (map f (tail lst)))))

(define (filter p? lst)
  (cond ((null? lst) '())
        ((p? (head lst)) (cons (head lst) (filter p? (tail lst))))
        (else (filter p? (tail lst)))))

;zad1
(define (take n lst)
  (cond ((or (zero? n) (null? lst)) '())
        ;((= 1 n) (head lst))
        (else (cons (head lst) (take (- n 1) (tail lst))))))

(define (drop n lst)
  (if (or (zero? n) (null? lst)) lst
      (drop (- n 1) (tail lst))))

;zad3-zip
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons ((cons (head lst1) (head lst2)))
             (zip (tail lst1) (tail lst2)))))
;zad4
(define (zipWIth f lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (f (head lst1) (head lst2)) (zipWith f (tail lst1) (tail lst2)))))

(define (zip1 lst1 lst2)
  (zipWith cons lst1 lst2))

;zad5
(define (one-elem-list? lst)
  (if (null? (tail lst)) #t #f))

(define (sorted? lst)
  (if (or (null? lst) (one-elem-list? lst)) #t
  (and (<= (head lst) (head (tail lst)))
       (sorted? (tail lst)))))

;zad6
(define (uniques lst)
  (define (helper newLst res)
    (cond ((null? newLst) '())
          ((member? (head newLst) (tail newLst)) (helper (tail newLst) res))
          (else (helper (tail newLst) (cons (head newLst) res)))))
  (helper lst '()))