(define (flip f)
  (lambda (x y) (f y x)))

(define (complement p?)
  (lambda (x) (not (p? x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define ^2 (lambda (x) (* x x))) ; (define (^2 x) (* x x))
(define 1+ (lambda (x) (+ x 1)))
(define id (lambda (x) x))

(define (repeat n f)
  (if (= n 0) id
      (compose f (repeat (- n 1) f))))