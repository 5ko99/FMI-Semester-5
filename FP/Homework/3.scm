; преобразува до низ
;(string #\a) --> "a"
; намира дължина на низ
;(string-length "abc") --> 3
; извлича елемент намиращ се на даден индекс
;(string-ref "abc" 1) --> #\b
; конкатенира два низа
;(string-append "abc" "def") --> "abcdef"
;(substring "abc" 1 3)
(define (is-digit? c)
  (cond ((eq? c #\0) #t)
        ((eq? c #\1) #t)
        ((eq? c #\2) #t)
        ((eq? c #\3) #t)
        ((eq? c #\4) #t)
        ((eq? c #\5) #t)
        ((eq? c #\6) #t)
        ((eq? c #\7) #t)
        ((eq? c #\8) #t)
        ((eq? c #\9) #t)
        (else #f)))

(define (is-sign? c)
  (cond ((eq? c #\+) #t)
        ((eq? c #\*) #t)
        ((eq? c #\-) #t)
        ((eq? c #\/) #t)
        ((eq? c #\^) #t)
        (else #f)))

(define (op-pr c)
  (cond ((string=? c "+") 0)
        ((string=? c "*") 1)
        ((string=? c "-") 0)
        ((string=? c "/") 1)
        ((string=? c "^") 2)
        (else -1)))

(define (is-space? c)
  (if (eq? c #\space)
      #t
      #f))



;0-> space or digit//in the beggining,after sign or space but before that sign
;1->sapce or digit or sign// after digit
;2-> sign or space//after space after digit

(define (expr-valid? str)
  (define (helper i next)
    (cond ((= (string-length str) i) #t)
          ((and (is-digit? (string-ref str i)) (< next 2)) (helper (+ i 1) 1))
          ((and (is-sign? (string-ref str i)) (> next 0)) (helper (+ i 1) 0))
          ((and (is-space? (string-ref str i)) (= next 1)) (helper (+ i 1) 2))
          ((and (is-space? (string-ref str i)) (= next 0)) (helper (+ i 1) 0))
          ((and (is-space? (string-ref str i)) (= next 2)) (helper (+ i 1) 2))
          (else #f)))
  (helper 0 0))

(define (remove-char str i)
  (string-append (substring str 0 i) (substring str (+ i 1) (string-length str))))

(define (clear-spaces str)
  (define (helper newStr i)
    (cond ((= (string-length newStr) i) newStr)
          ((is-space? (string-ref newStr i)) (helper (remove-char newStr i) (+ i 0)))
          (else (helper newStr (+ i 1)))))
  (helper str 0))

(define sep ",")

(define (string-reverse str)
  (define (loop i res)
    (if (< i 0)
        res
        (loop (- i 1) (string-append  res (make-string 1 (string-ref str i))))
        ))
  (loop (- (string-length str) 1) ""))


(define (expr-rp expr)
  (expr-rp-helper expr))

(define (expr-rp-helper exprI)
  (expr-valid? exprI)
  (define expr (clear-spaces exprI))
  (define (helper new-signs token)
                  (if (or (is-empty? new-signs) (< (op-pr (top new-signs)) (op-pr token)))
                      (begin (display sep) (push new-signs token))
                      (begin (display (top new-signs)) (helper (pop new-signs) token))))
  
  (define (loop i signs)
    (define token (if (< i (string-length expr))
                      (make-string 1 (string-ref expr i))
                      -1))
    
    (cond  ((= i (string-length expr)) (display signs))
           ((is-sign? (string-ref expr i))
            (begin  (if (or (is-empty? signs) (> (op-pr token) (op-pr (top signs)) ))
                (begin (display sep) (loop (+ i 1) (push signs token)))
                (loop (+ i 1) (helper signs token)))))
           (else (begin (display token) (loop (+ i 1) signs (string-append (make-string 1 (string-ref expr i))))))
           ))
  (loop 0 ""))

;stack
(define (top s)
  (if (is-empty? s)
      -1
      (make-string 1 (string-ref s 0))))

(define (push s c)
  (string-append c s))

(define (is-empty? s)
  (zero? (string-length s)))

(define (pop s)
  (if (is-empty? s)
      -1
      (substring s 1 (string-length s))))
  