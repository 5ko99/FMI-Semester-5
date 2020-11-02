;accumulate function
(define (accumulate op term init a next b)  
  (define (loop i)
      (if (<= i b)
          (op (term i) (loop (next i)) )
          init
  ))
  (loop a)
)

;accumulate function с която върви наобратно. Т.е. от по-голяма стойност с -1 към по-малка.
;Нужна ми е за чертаенето на квадрата използвайки само акм., защото аз първо чертая рекурсивно най външният,
;след това по малките, т.е. вървя от стойност на i от n до 1.
(define (accumulate1 op term init a next b)  
  (define (loop i)
      (if (>= i a)
          (op (term i) (loop (next i)) )
          init
  ))
  (loop b)
)
;printing simple functions
(define (dash x y) (display #\─))
(define (vert x y) (display #\│)
(display #\space))
(define (vertAfter x y) (display #\space)
(display #\│))
;closure of accumulate-нормалната версия която върви от a до b.
;Тя ни е нужна за печатене на хоризонталните черти, а също така и за вертикалните черти със спейс преди тях,
;или  тези с спейс след тях
(define (print f a b)
  (define (id x) x)
  (define (1+ x) (+ x 1))
  (accumulate f id 0 a 1+ b))

;Решение при което не се извикваме рекурсивно, а извикваме акм.
(define (solution n)
  (define (a k)
         (print vert 1 (- n k))
         (display #\┌)
         (print dash 1 (- (* 4 k) 3))
         (display #\┐)
         (print vertAfter 1 (- n k))
         (display #\newline)
         k)
  (define (b k junk)
         (print vert 1 (- n k))
         (display #\└)
         (print dash 1 (- (* 4 k) 3))
         (display #\┘)
         (print vertAfter 1 (- n k))
         (display #\newline))
  (define (1- x) (- x 1))
  (accumulate1 b a (display "") 1 1- n))

;Решение при което се викаме рекурсивно. Резултатът е същият като и при другата функция.
(define (solutionAlternative n)
  (define (helper k)
    (if (> k 0)
        (begin
         (print vert 1 (- n k))
         (display #\┌)
         (print dash 1 (- (* 4 k) 3))
         (display #\┐)
         (print vertAfter 1 (- n k))
         (display #\newline)
         (helper (- k 1))
         (print vert 1 (- n k))
         (display #\└)
         (print dash 1 (- (* 4 k) 3))
         (display #\┘)
         (print vertAfter 1 (- n k))
         (display #\newline))
        (display "")))
  (helper n))

;Функцията извиква решението при което не се извикваме рекурсивно,
;а използваме акм.
(define (squares n)
  (solution n))