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
           (cond [(not node?) #f];leef before node
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
  (trace loop)
  (if (eq? (string-ref str 0) #\{) (loop 1 #f #f #f final?) #f)
  )
(trace tree?-helper)

(define (tree? input)
  (let((result (tree?-helper input #t)))
    (if (not result) #f #t)))

(provide (all-defined-out))
  
  