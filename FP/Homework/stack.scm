(define sep ",")

(define (push s el)
  (string-append el sep s))

(define (top s)
  (define (loop i res)
    (if (or (= i (string-length s)) (string=? (make-string 1 (string-ref s i)) sep))
        res
        (loop (+ i 1) (string-append res (make-string 1 (string-ref s i))))
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
          ((string=? (make-string 1 (string-ref s i)) sep) (loop (+ i 1) (+ count 1)))
          (else (loop (+ i 1) count))))
  (loop 0 0))
  

(define (is-empty? s)
  (zero? (string-length s)))

(define (top-top s)
  (top (pop s)))

(define (pop-pop s)
  (pop (pop s)))