(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (id x) x)
(define (1+ x) (+ 1 x))
(define (2+ x) (+ 2 x))

;zad 1
(define (!! n)
  (accumulate * 1 (if (odd? n) 1 2) n id 2+))

;zad2
(define (! n)
  (accumulate * 1 1 n id 1+))
(define (nchk n k)
  (/ (! n) (* (! (- n k)) (! k))))

;zad3
; (nchk n k) = n*(n-1)*...*(n-k+1) / k*(k-1)*...*1
(define (nchk* n k)
  (accumulate *
              1
              0 (- k 1)
              (lambda (i) (/ (- n i) (- k i)))
              1+))

;zad4
(define (2^ n)
  (accumulate * 1 1 n (lambda (x) 2) 1+))
;alternativly
(define (2^* n)
  (accumulate + 0 0 n (lambda (k) (nchk* n k)) 1+))

;zad5
(define (all? p? a b)
  (if (> a b) #t
      (and (p? a)
           (all? p? (+ a 1) b))))

(define (complement p?)
  (lambda (x) (not (p? x))))

(define (any? p? a b)
  (not (all? (complement p?) a b)))

;filter acc
(define (filter-accum p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accum p? op nv (next a) b term next)))
        (else       (filter-accum p? op nv (next a) b term next))))

;zad6
(define (divisor-sum n)
  (filter-accum (lambda (x) (zero? (remainder n x))) + 0 1 n id 1+))

;zad7
(define (count p? a b)
  (filter-accum p? + 0 a b (lambda (x) 1) 1+))

;zad8 prime
(define (prime? n)
  (and (> n 1)
   (zero? (count (lambda (x) (zero? (remainder n x))) 2 (sqrt n)))))

;compose func n times
(define (compose f g)
    (lambda (x) (f (g x))))
(define (repeat f n)
  (accumulate compose id 1 n (lambda (x) f) 1+))

(define upLeft "\u250C")
(define upRight "\u2510")
(define downLeft "\u2514")
(define downRight "\u2518")
(define space " ")

(define (e^x x n)
  (accumulate + 0 0 n (lambda (i) (/ (expt x i) (! i))) 1+)
  )

(define (sum-of-digits n)
  (define (loop n1 sum)
    (if (= n1 0)
        sum
        (loop (quotient n1 10) (+ sum (remainder n1 10)))
        )
    )
  (loop n 0))

(define (arm a b)
  (filter-accum (lambda (x) (odd? (sum-of-digits x))) + 0 a b id 1+)) 
        