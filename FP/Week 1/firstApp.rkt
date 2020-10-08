(define x 5)
(define (1+ x) (+ x 1))
(+ 2 3 (* 2 2) (1+ 10))
(define % remainder)
(define q quotient)

(define (fact n)
  (if (< n 2)
      n
      (* n (fact (- n 1)))
      ))
(display "\u03BB-calculas \n")


(define (fact n)
   (define (helper current result)
       (if (> current 0)
           (helper (- current 1) (* result current))
           result
           )
     )
     (helper n 1)
  )

(fact 5)
