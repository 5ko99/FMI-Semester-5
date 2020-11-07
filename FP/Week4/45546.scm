;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 1
; 2020-11-07
;
; Начален час на контролното: 09:00
; Име: Петко Красимиров Каменов
; ФН: 45546
; Специалност: Информатика
; Курс: Трети
; Административна група: Първа 
;

(define (toBinary n)
  (define (loop curN res)
    (if (= curN 0)
        res
        (loop (quotient curN 2) (string-append (number->string (remainder curN 2)) res))
              ))
  (loop n ""))

(define (set-contains? s el)
  (let* ( (binaryS (toBinary s))
          (len (string-length binaryS)))
    (if (and (>= (- len (+ el 1)) 0) (>= el 0)) 
        (eq? (string-ref binaryS (- len (+ el 1))) #\1)
        #f
        )
    )
  )

(define (nset-add s elem)
  (if (set-contains? s elem)
      s
      (+ s (expt 2 elem))
      )
  )

(define (nset-remove s elem)
  (if (set-contains? s elem)
      (- s (expt 2 elem))
      s
      )
  )

(define (set-size s)
  (define bin-s (string->number (toBinary s)))
  (define (loop newS n)
    (if (zero? newS)
        n
        (if (= (remainder newS 10) 1)
            (loop (quotient newS 10) (+ n 1))
            (loop (quotient newS 10) n))))
  (loop bin-s 0))

(define (nset-contains-its-size s)
  (define s-size (set-size s))
  (set-contains? s s-size))


(define (set-contains-with-bin? binaryS el)
  (let* (
          (len (string-length binaryS)))
    (if (and (>= (- len (+ el 1)) 0) (>= el 0)) 
        (eq? (string-ref binaryS (- len (+ el 1))) #\1)
        #f
        )
    )
  )

(define (nset-map s f)
  (define bin-s (toBinary s))
  (define (loop i res)
    (cond ((= i (string-length bin-s)) res)
          ((set-contains-with-bin? bin-s i) (loop (+ i 1) (nset-add res (f i))))
          (else (loop (+ i 1) res))
          )
    )
  (loop 0 0)
  )
    