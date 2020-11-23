#lang racket
(require racket/trace)

(define head car)
(define tail cdr)

(define (is-space? c)
  (if (eq? c #\space)
      #t
      #f))

(define (is-digit? c)
  (cond ((eq? c #\0) #t)
        ((eq? c #\1) #t)
        ((eq? c #\2) #t)
        ((eq? c #\3) #t)
        ((eq? c #\4) #t)
        ((eq? c #\5) #t)
        ((eq? c #\6) #t)
        ((eq? c #\7) #t)
        ((eq? c #\8) #t)
        ((eq? c #\9) #t)
        (else #f)))


(define (tree?-helper input final?)
  (define str (string-normalize-spaces input))
  (define (loop i node? left? right? final?)
    (if (<= (string-length str) i) #f
    (let ((cur (string-ref str i)))
    (cond [(and (<= (string-length str) (+ i 1)) (eq? cur #\})) (if (and node? left? right?) (+ i 1) #f)]
          [(<= (string-length str) (+ i 1)) #f];if you reach end but not }
          [(and (is-space? cur) (is-digit? (string-ref str (- i 1)))) (loop (+ i 1) #t left? right? final?)]
          [(is-space? cur) (loop (+ i 1) node? left? right? final?)]
          [(and (is-digit? cur) (not node?)) (loop (+ i 1) node? left? right? final?)]
          [(and (is-digit? cur) node?) #f];veche e procheteno node, a pak se poqvqva chislo
          [(eq? cur #\*)
           (cond [(and (not node?) (not (is-digit? (string-ref str (- i 1))))) #f];leef before node
                 [(not left?) (loop (+ i 1) #t #t #f final?)]
                 [(and left? (not right?)) (loop (+ i 1) #t #t #t final?)]
                 [else #f])]
          [(eq? cur #\{)
           (let ((len (tree?-helper (substring str (+ i 0)) #f)))
             (cond [(not node?) #f];leef before node
                   [(not left?) (if len (loop (+ i len) #t #t #f final?) #f)]
                   [(and left? (not right?)) (if len (loop (+ i len) #t #t #t final?) #f)]
             [else #f]
             ))]
          [(eq? cur #\}) (if (and node? left? right?) (if (not final?) (+ i 1) #f) #f)]
          ;[(eq? cur #\}) (and node? left? right?)]
          [else #f]
  ))))
;  (trace loop)
  (if (zero? (string-length str)) #f
  (if (eq? (string-ref str 0) #\{) (loop 1 #f #f #f final?) (if (and (= (string-length str) 1) (eq? (string-ref str 0) #\*)) #t #f))
  ))
;(trace tree?-helper)

;helper function for work with trees
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define (length lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (tail lst)))]))

(define (string->tree-helper str)
  (define (loop i node left right lset rset)
    (let ((cur (string-ref str i)))
    (cond [(eq? cur #\}) (cons (make-tree (string->number node) left right) i)]
          [(is-digit? cur) (loop (+ i 1) (string-append node (string cur)) left right lset rset)]
          [(eq? cur #\*) (if (not lset) (loop (+ i 1) node empty-tree right #t #f) (loop (+ i 1) node left empty-tree #t #t))]
          [(is-space? cur) (loop (+ i 1) node left right lset rset)]
          [(eq? cur #\{)
           (let* ((treePair (string->tree-helper (substring str i)))
                 (tree (head treePair))
                 (len (tail treePair)))
             (if (not lset)
                 (loop (+ i len 1) node tree right #t #f)
                 (loop (+ i len 1) node left tree #t #t)))]
          )))
  (loop 1 "" `() `() #f #f)
  )



(define (tree? input)
  (let((result (tree?-helper input #t)))
    (if (not result) #f #t)))

(define (string->tree input)
  (define str (string-normalize-spaces input))
  (if (and (= (string-length str) 1) (eq? (string-ref str 0) #\*)) empty-tree 
  (if (tree? str) (head (string->tree-helper str)) #f)
  ))

(string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
(string->tree "{1 * {3 {111 * *} *}}")
(string->tree "{11515 {5 {6 {7 * *} *} {100 * *}} *}")
(string->tree "*")

(provide (all-defined-out))
  
  