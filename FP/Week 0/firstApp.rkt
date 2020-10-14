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

(define (fib n)
  (define (helper n-1 n-2 curr limit)
      (if (> curr limit)
      n-1
      (helper (+ n-1 n-2) n-1 (+ curr 1) limit)
      
    ))
   (helper 1 1 3 n)
  )
(fib 8)

(define (sum-int x y)
  (if(> x y) 0
   (+ x (sum-int (+ x 1) y)))
)

(define (pow x n)
  (if (= n 0) 1
  (* x (pow x (- n 1)))
  ))

(define (a n)
  (if (< n 0) (- n)
  n
  ))
(a -8)

(define (isPrime? n)
  (define (helper current-number)
    (cond
      ((>= current-number (/ n 2)) #t)
      ((= (remainder n current-number) 0) #f)
      (else (helper (+ current-number 1)))
      )
    )
  (if (< n 2)
      #f
      (helper 2)
      )
)

(isPrime? 6)
(isPrime? 2)
(isPrime? 7)
(isPrime? 66)
(isPrime? 101)