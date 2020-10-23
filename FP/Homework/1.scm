(define (accumulate-i op term init a next b)
  (define (loop i result)
    (if (> i b) result
        (loop (next i) (op result (term n i)) )
        ))
  (loop a init)
  )

;(string-append "\u250C" (make-string (* n n) #\u2500) "\u2510"))
;(string-append "\u2514" (make-string (* n n) #\u2500) "\u2518"))
(define (make-row n i)
  (define (helper j res)
    
  )



(define (squares n)
  (accumulate-i string-append make-row "" 0 + (* 2 n)))
  