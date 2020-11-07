;функция която превръща число от десетична бройна система към двоична
(define (toBinary n)
  (define (helper n res pos)
    (if (= n 0) res
        (helper (quotient n 2)
                (+ res (* (remainder n 2) (expt 10 pos)))
                (+ pos 1))))
  (helper n 0 0))

;функция която превръща число от двоична бройна система към десетична
(define (toDecimal n)
  (if (= n 0) 0
      (+ (remainder n 10) (* 2 (toDecimal (quotient n 10))))))

;добавя елемента elem в мновеството set
(define (set-add set elem)
  (+ set (expt 2 elem)))

;премахва елемента елем от мновеството set
(define (set-remove set elem)
  (- set (expt 2 elem)))

;проверява дали elem се съдържа в мн. set
(define (set-contains? set elem)
  (define asBin (toBinary set))
  (define (helper setN i)
    (cond ((set-empty? setN) #f)
          ((= i elem) (if (= (remainder setN 10) 1) #t #f))
          (else (helper (quotient setN 10) (+ i 1)))
        ))
  (helper asBin 0))

;проверява дали set е празното множество
(define (set-empty? set)
  (if (zero? set) #t #f))

;помощни функция
(define (one? x)
  (if (= x 1) #t #f))
(define (id x) x)
(define (double x) (+ x x))

;функция която връща мощността на множеството set
(define (set-size set)
  (define asBin (toBinary set))
  (define (helper setNew count)
    (cond ((set-empty? setNew) count)
          ((zero? (remainder setNew 10)) (helper (quotient setNew 10) count))
          ((one? (remainder setNew 10)) (helper (quotient setNew 10) (+ count 1)))))
  (helper asBin 0))

;сечение на две множества A и Б
(define (set-intersect A B)
  (define AasBin (toBinary A))
  (define BasBin (toBinary B))
  (define (helper An Bn res pos)
    (cond ((or (set-empty? An) (set-empty? Bn)) (toDecimal res))
          ((and (one? (remainder An 2)) (one? (remainder Bn 2)) (helper (quotient An 10) (quotient Bn 10) (+ res (* 1 (expt 10 pos))) (+ pos 1))))
          (else (helper (quotient An 10) (quotient Bn 10) (+ res (* 0 (expt 10 pos))) (+ pos 1)))))
    (helper AasBin BasBin 0 0))

;обединение на две множества A и Б
(define (set-union A B)
  (define AasBin (toBinary A))
  (define BasBin (toBinary B))
  (define (helper An Bn res pos)
    (cond ((and (set-empty? An) (set-empty? Bn)) (toDecimal res))
          ((or (one? (remainder An 2)) (one? (remainder Bn 2))) (helper (quotient An 10) (quotient Bn 10) (+ res (* 1 (expt 10 pos))) (+ pos 1)))
          (else (helper (quotient An 10) (quotient Bn 10) (+ res (* 0 (expt 10 pos))) (+ pos 1)))))
    (helper AasBin BasBin 0 0))

;разлика на множествата A и Б
(define (set-difference A B)
  (define AasBin (toBinary A))
  (define BasBin (toBinary B))
  (define (helper An Bn res pos)
    (cond ((and (set-empty? An) (set-empty? Bn)) (toDecimal res))
          ((and (one? (remainder An 2)) (zero? (remainder Bn 2))) (helper (quotient An 10) (quotient Bn 10) (+ res (* 1 (expt 10 pos))) (+ pos 1)))
          (else (helper (quotient An 10) (quotient Bn 10) (+ res (* 0 (expt 10 pos))) (+ pos 1)))))
    (helper AasBin BasBin 0 0))


;функция която по дадено множество set и друга функция p, която връща число на всеки един елемент от него,
;изчислява общата цена на мн. set (сумира p-тата на всички елементи в множеството)
(define (set-price set p)
    (define asBin (toBinary set))
  (define (helper pos setNew price)
    (cond ((set-empty? setNew) price)
          ((zero? (remainder setNew 10)) (helper (+ pos 1) (quotient setNew 10) price))
          ((one? (remainder setNew 10)) (helper (+ pos 1) (quotient setNew 10) (+ price (p pos))))))
  (helper 0 asBin 0)
 )

;функция която по дадено c-капацитет на раницата; n-брой елементи; w-функция която връща теглото на определен елемент
;p-функция която връща цената; решева задачата за раницата, като връща оптималното множество от елементи които да вземем.
;за да видим цената му, може просто да извикаме върху вече върнатият резултат функцията set-price върху това множество
;и с функцията за цена която подаваме и на knapsack
(define (knapsack c n w p)
  (define (helper i set curPrice capLeft)
    (cond ((>= i n) set)
          ((> (w i) capLeft) (helper (+ i 1) set curPrice capLeft))
          (else (let (
                      (set1 (helper (+ i 1) (set-add set i) (+ curPrice (p i)) (- capLeft (w i))))
                      (set2 (helper (+ i 1) set curPrice capLeft))
                      )
                  (if (> (set-price set1 p) (set-price set2 p))
                      set1
                      set2)
                  )
                ))) 
  (helper 0 0 0 c))