;1
(define (toBinary n)
  ;Инжарианта: pos е индексът на този бит от резултата, който
  ;ще получим преди деление на n на 2
  (define (helper n res pos)
    (if (= n 0) res
        (helper (quotient n 2)
                (+ res (* (remainder n 2) (expt 10 pos)))
                (+ pos 1))
    ))
  (helper n 0 0))
(toBinary 255)

;lambda
(define fma (lambda (x y z) (+ x (* y z))))

;3
(define (constantly c)
  (lambda (x) c))

(define forever21 (constantly 21))
((constantly 21) 5)

;4
(define (flip f)
  (lambda (x y) (f y x)))

(define fliped- (flip -))

;5
(define (complement f)
  (lambda (x) (not (f x))))

(define (less-than-5? x) (< x 5))
(define f (complement less-than-5?))

;6
(define (compose f g) ;(f.g)(x) = f(g(x))
  (lambda (x) (f (g x))))

(define ^2 (lambda (x) (* x x)))
(define 1+ (lambda (x) (+ x 1)))
(define f (compose 1+ ^2))
(f 6)

(define id (lambda (x) x))

;7
(define (repeat n f)
  (if (= n 0) id
      (compose f (repeat (- n 1) f))
      ;(lambda (x)
      ;  (f ((repeat (- n 1) f) x)))
      ))

(define f (repeat 5 1+))
(f 10)
;8
(define dx 0.1)
(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))
((derive ^2) 5)

;9
(define (derive-n n f)
  (if (= n 0) f
      (derive (derive-n (- n 1) f))))
((derive-n 2 ^2) 5)

(define (derive-n* n f)
  ((repeat n derive) f))
((derive-n* 2 ^2) 5)