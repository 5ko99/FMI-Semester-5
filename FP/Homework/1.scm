(define (filter-accumulate p? op term init a next b)
  (define (loop i)
    (cond ((> i b) init)
          ((p? i) (op (term i) (loop (next i))) )
          (else (loop (next i)))
          ))
  (loop a)
  )

(define (print-row currRow currCol n)
  (filter-accumulate (lambda (x) ()) 


(define (squares n)
  