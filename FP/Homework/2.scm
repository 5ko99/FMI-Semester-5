(define (toBinary n)
  (define (helper n res pos)
    (if (= n 0) res
        (helper (quotient n 2)
                (+ res (* (remainder n 2) (expt 10 pos)))
                (+ pos 1))))
  (helper n 0 0))

(define (set-add set elem)
  (+ set (expt 2 elem)))

(define (set-remove set elem)
  (- set (expt 2 elem)))

;to add check if set is empty
(define (set-contains? set elem)
  (define asBin (toBinary set))
  (define (helper setN i)
    (cond ((set-empty? setN) #f)
          ((= i elem) (if (= (remainder setN 10) 1) #t #f))
          (else (helper (quotient setN 10) (+ i 1)))
        ))
  (helper asBin 0))

(define (set-empty? set)
  (if (zero? set) #t #f))

(define (one? x)
  (if (= x 1) #t #f))

(define (set-size set)
  (define asBin (toBinary set))
  (define (helper setNew count)
    (cond ((set-empty? setNew) count)
          ((zero? (remainder setNew 10)) (helper (quotient setNew 10) count))
          ((one? (remainder setNew 10)) (helper (quotient setNew 10) (+ count 1)))))
  (helper asBin 0))
    