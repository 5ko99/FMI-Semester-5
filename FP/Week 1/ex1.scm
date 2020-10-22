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

(reverse-int 1234)

;Zad 5
(define (palindrome? n)
  (= n (reverse-int n)))

(palindrome? 313)
(palindrome? 123)

;Zad 8
(define (prime? n)
  (define (helper i)
    (cond ((> i (/ n 2)) #t)
          ((not(= (remainder n i) 0)) (helper (+ i 1)))
          (else #f)))
  (if (> n 1)
    (helper 2)
    #f))
(prime? 97)

;helper for getting the first digit
(define (getFirstDigit n)
  (if (>= n 10)
      (getFirstDigit (quotient n 10))
      n))

;Zad 9
(define (increasing? n)
  (define (helper newN last before-last)
    (cond((= newN 0) #t)
         ((> last before-last) (helper (quotient newN 10) before-last (last-digit (quotient newN 100))))
         (else #f)))
  (helper n (last-digit n) (last-digit (quotient n 10))))

(increasing? 1235468)
(increasing? 12489)
(increasing? 4456)


;Zad 11
(define (toDecimal n)
  (define (helper newN acc pow)
    (cond((<= newN 0) acc)
         (else (helper (quotient newN 10) (+ acc (* (last-digit newN) (fastPow 2 pow))) (+ pow 1)))))
  (helper n 0 0))

(toDecimal 101010)



;Zad 10
(define (toBinary n)
  (define (helper newN res)
    (cond((zero? newN) res)
         (else (helper (quotient newN 2) (cons (remainder newN 2) res)))))
  (helper n '()))

(toBinary 8) ;-> 1000
(toBinary 15) ;-> 1111
(toBinary 42) ;-> 101010
