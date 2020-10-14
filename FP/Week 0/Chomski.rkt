#lang racket

(define (sum-int x y)
  (if(> x y) 0
   (+ x (sum-int (+ x 1) y)))
)

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1))))
 )

(define (pow x y)
  (if(= y 0) 1
  (* x (pow x (- y 1))))
)

(define (isPos? x)
  (> x 0)
)

(define (notZero? n)
  (not (if (= n 0)
      #t
      #f))
  )

(define (isPrime? n)
  (define (helper current-number)
    (cond
      [(>= current-number n) #t]
      [(= (remainder n current-number) 0) #f]
      [else (helper (+ current-number 1))]
      )
    )
  (if (< n 2)
      #f
      (helper 2)
      )
)

(equal? (isPrime? 6) #f)
(equal? (isPrime? 1) #f)
(equal? (isPrime? 2) #t)

(define (isRoot? n)
  (if(= (- (* 3 (* n n)) (+ (* 2 n) 1)) 0)
     #t
     (print "not root")
  )
)

(define (abs n)
  (cond [(< n 0) (- n)]
        [else n]
   )
)




  









  