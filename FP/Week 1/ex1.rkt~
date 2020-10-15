;Zad 6
(define (divisors-sum n)
  (define (helper i sum)
    (cond ((> i n) sum)
          ((= 0 (remainder n i))
             (helper (+ i 1) (+ sum i)))
          (else (helper (+ i 1) sum))))
  (if (and (positive? n)
           (integer? n))
      (helper 1 0)
      #f))

(divisors-sum 12)

;Zad 3
(define (last-digit n)
  (remainder n 10))

(define (count-digit d n)
  (define (helper newN count)
    (cond ((= newN 0) count)
          ((= d (last-digit newN))
            (helper (quotient newN 10) (+ count 1)))
          (else (helper (quotient newN 10) count))))

  (if (= n 0) (if (= d 0) 1 0)
  (helper n 0)))

(count-digit 3 3313323)

;Zad 1
(define (sumInInterval a b)
  (define (helper curr sum)
    (if (> curr b) sum
    (helper (+ curr 1) (+ sum curr))))
  (if (<= a b) (helper a 0)
  #f)
  )
(sumInInterval 1 10)

;Zad 2
(define (sq x) (* x x))

(define (fastPow x n)
  (cond ((= n 0) 1)
    ((= (remainder n 2) 0) (sq (fastPow x (quotient n 2))))
    (else (* x (sq (fastPow x (quotient n 2)))))))
(fastPow 21 3)

;Zad7
(define (perfect? n)
  (= (divisors-sum n) (* 2 n)))
(perfect? 33550336)
(perfect? 4)

;Zad 4
(define (reverse-int n)
  (define (helper newN res)
    (cond ((= newN 0) res)
          (else (helper (quotient newN 10) (+ (* res 10) (last-digit newN))))))
  (helper n 0))

(reverse-int 123)

;Zad 5
(define (palindrome? n)
  (= n (reverse-int n)))

(palindrome? 313)
(palindrome? 123)

;Zad 8
(define (prime? n)
  (define (helper i)
    (cond ((= i n) #t)
          ((\= (remainder n i) 0) (helper (+ i 1)))
          (else #f)))
  (helper 2))
(prime? 97)

      

        