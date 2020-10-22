;Accumulate with condition
(define (accumulate op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)) )
        init
        ))
  (loop a)
  )

;Acumulate without condition
(define (accumulate-i init op a next b)
  (define (loop result i)
    (if (<= i b)
        (loop (op result i) (next i))
        result
        ))
  (loop init a)
  )

(define (1+ x) (+ x 1))

(accumulate-i 1 * 1 1+ 10)

(define (id x) x)
(define (1+ n) (+ n 1))
(define (sum-int a b)
  (accumulate + id 0 a 1+ b)
)

(define (sum-row base a b)
  (define (1+ n) (+ n 1))
  (define (term t) (expt base t))
  (accumulate + term 0 a 1+ b)
  )

(define (sum-even a b)
  (accumulate +
              id
              0
              (if (even? a) a (+ a 1))
              (lambda (n) (+ n 2))
              b))


;Filter one
(define (filter-accumulate p? op term init a next b)
  (define (loop i)
    (cond ((> i b) init)
          ((p? i) (op (term i) (loop (next i))) )
          (else (loop (next i)))
          ))
  (loop a)
  )

(define (sum-op a b)
  (filter-accumulate
   even?
   +
   id
   0
   a
   1+
   b)
  )


