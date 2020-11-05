;Помощни функции и константи
(define sep ",")
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

(define (is-operand? c)
  (cond ((string=? c "+") #t)
        ((string=? c "*") #t)
        ((string=? c "-") #t)
        ((string=? c "/") #t)
        ((string=? c "^") #t)
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

(define (string->procedure c)
  (cond ((string=? c "+") (lambda (a b) (+ a b)))
        ((string=? c "*") (lambda (a b) (* a b)))
        ((string=? c "-") (lambda (a b) (- a b)))
        ((string=? c "/") (lambda (a b) (/ a b)))
        ((string=? c "^") (lambda (a b) (expt a b)))
        (else #f)))

;stack имплементация, леко модифициран, защото имаме достъп и до вторият параметър на върха,
;това го правя за улеснение при изчисиление на израз, когато трябва да вземем най-горните 2 елемента
;от върха на стека. Наистина ненужно е, но с цел по добра четимост
(define (push s el)
  (string-append el sep s))

(define (top s)
  (define (loop i res)
    (if (or (= i (string-length s)) (string=? (string (string-ref s i)) sep))
        res
        (loop (+ i 1) (string-append res (string (string-ref s i))))
        ))
  (if (is-empty? s) #f
      (loop 0 "")))

(define (pop s)
  (define len (string-length (top s)))
  (if (one-elem? s)
      ""
      (substring s (+ len 1) (string-length s))))

(define (one-elem? s)
   (define (loop i count)
    (cond ((>= count 2) #f)
          ((= i (string-length s)) #t)  
          ((string=? (string (string-ref s i)) sep) (loop (+ i 1) (+ count 1)))
          (else (loop (+ i 1) count))))
  (loop 0 0))
  

(define (is-empty? s)
  (zero? (string-length s)))

(define (top-top s)
  (top (pop s)))

(define (pop-pop s)
  (pop (pop s)))


;функция за премахване на символ в низ по дадена позиция
(define (remove-char str i)
  (string-append (substring str 0 i) (substring str (+ i 1) (string-length str))))

;Помощна функция за почистване на ненужните спейсове от израз
;извиквам я, само след проверка дали даден израз е валиде, тоест тя предполага,
;че работи с валиден такъв
(define (clear-spaces str)
  (define (helper newStr i)
    (cond ((= (string-length newStr) i) newStr)
          ((is-space? (string-ref newStr i)) (helper (remove-char newStr i) (+ i 0)))
          (else (helper newStr (+ i 1)))))
  (helper str 0))

;функция за изчисление на токена
(define (tokenEval i j cur expr)
      (cond ((>= j (string-length expr)) cur)
            ((and (= i j) (is-operand? (string (string-ref expr j)))) (string (string-ref expr j)))
            ((and (not (= i j)) (is-operand? (string(string-ref expr j)))) cur)
            (else (tokenEval i (+ j 1) (string-append cur (string (string-ref expr j))) expr))))

;Помощна функция в която съм изнесъл логиката на expr-rp
(define (expr-rp-helper exprI)
  (define expr (clear-spaces exprI))
  (define (loop i signs res)
    (define token (tokenEval i i "" expr))
    (cond  ((= i (string-length expr)) (string-append res signs))
           ((is-operand? token)
            (if (or (is-empty? signs) (> (op-pr token) (op-pr (top signs)) ))
                (loop (+ i 1) (push signs token) (string-append res sep))
                (loop i (pop signs) (string-append res (top signs)))))
           (else (loop (+ i (string-length token)) signs (string-append res token )))))
  (loop 0 "" ""))

;Функционалността за изчисление на израз
(define (expr-eval-helper exprI)
  (define expr (clear-spaces exprI))
  (define (loop i signsStack valuesStack)
    (define token (tokenEval i i "" expr))
    (cond ((or (and (= i (string-length expr)) (not (is-empty? signsStack))) (is-operand? token))
           (if (or (is-empty? signsStack) (> (op-pr token) (op-pr (top signsStack))))
               (loop (+ i 1) (push signsStack token) valuesStack)
               (let* ((A (string->number (top valuesStack)))
                      (B (string->number (top-top valuesStack)))
                      (op (top signsStack))
                      (result ((string->procedure op) B A)))
                 (loop i (pop signsStack) (push (pop-pop valuesStack) (number->string result))))
               ))
          ((= i (string-length expr))
           (top valuesStack))
          (else (loop (+ i (string-length token)) signsStack (push valuesStack token)))))
  (loop 0 "" ""))


;Основни функции
(define (expr-rp expr)
  (if (expr-valid? expr)
      (let* ((res (expr-rp-helper expr)))
             (substring res 0 (- (string-length res) 1)))
      (#f)))

;0-> space or digit//in the beggining,after sign or space but before that sign
;1->sapce or digit or sign// after digit
;2-> sign or space//after space after digit

(define (expr-valid? str)
  (define (helper i next canEnd? )
    (cond ((= (string-length str) i) canEnd?)
          ((and (is-digit? (string-ref str i)) (< next 2)) (helper (+ i 1) 1 #t))
          ((and (is-operand? (string (string-ref str i))) (> next 0)) (helper (+ i 1) 0 #f))
          ((and (is-space? (string-ref str i)) (= next 1)) (helper (+ i 1) 2 canEnd?))
          ((and (is-space? (string-ref str i)) (= next 0)) (helper (+ i 1) 0 canEnd?))
          ((and (is-space? (string-ref str i)) (= next 2)) (helper (+ i 1) 2 canEnd?))
          (else #f)))
  (helper 0 0 #t))

(define (expr-eval expr)
  (cond ((not (expr-valid? expr)) #f)
        ((zero? (string-length expr)) 0)
        (else (expr-eval-helper expr))
        ))




  


  