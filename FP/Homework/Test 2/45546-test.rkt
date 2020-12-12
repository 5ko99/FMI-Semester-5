#lang racket/base
;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 2
; 2020-12-12
;
; Име: Петко Каменов
; ФН: 45546
; Специалност: Информатика
; Курс: Трети
; Административна група: Първа 
; Начален час на контролното: 8:00
;



(require "45546.rkt") ; Променете името на файла, който се включва така,
                   ; че тук да се зарежда файлът с вашето решение.
                   ; Например ако файлът с решението ви се казва 12345.rkt,
                   ; променете реда на (require "12345.rkt").

(require rackunit rackunit/gui)

(test/gui

 ; Даденият по-долу пример е само ориентировъчен.
 ; Когато решавате задачите си, можете да го изтриете
 ; или да го промените така, че да проверява условия
 ; свързани с вашия код.
 
 (test-suite
  "sample-function"

  (test-true   "sample test 1" (sample-function #t))
  (test-false  "sample test 2" (sample-function #f))
  (test-equal? "sample test 3" (sample-function 10) 10)
  (test-equal? "sample test 4" (sample-function '(1 2 3)) '(1 2 3))

  (test-case
   "A sample test case, which performs multiple checks"
   (check-true   (sample-function #t))
   (check-false  (sample-function #f))
   (check-equal? (sample-function 10) 10))
  )

  
  ;my tests

  ;1st task
 (test-suite
  "1st task"
  (test-true "empty-tree" (empty-tree? '()))
  (test-false "non-empty-tree" (empty-tree? '(2 () ())))
  (test-true "leaf" (is-leaf? '(5 () ())))
  (test-false "not-leaf" (is-leaf? '()))

  (test-equal? "3 trees-with-one" (tree-with-one-suc '(5 () (4 () (6 () ())))) 2)
  (test-equal? "2 trees-with-two" (tree-with-two-suc '(5 (6 () ()) (4 (8 () ()) (6 () ()) ))) 2)
  (test-equal? "3 trees-with-one" (tree-with-one-suc '(45 (6 (3 (5 () ()) ()) ()) (8 (336 () ()) ()))) 3)
  (test-equal? "1 trees-with-two" (tree-with-two-suc '(45 (6 (3 (5 () ()) ()) ()) (8 (336 () ()) ()))) 1)

  (test-true "Deg. tree" (degenerate? '(5 (8 (6 () ()) (4 () ())) (10 (8 () (6 () (11 () ()))) ()))))
  (test-true "Deg. tree" (degenerate? '()))
  (test-true "Deg. tree" (degenerate? '(45 (6 (3 (5 () ()) ()) ()) (8 (336 () ()) ())) ))
  (test-false "Non-Deg. tree" (degenerate? '(8 () ())))
  (test-false "Non-Deg. tree" (degenerate? '(4 (3 (2 () ()) ()) (5 () ()))))
  )

  ;2nd task
 (test-suite
  "2nd task"
  (test-equal? "(cons (cons -3 3) (cons 4 -3))" (cover (list (cons -2 3)
             (cons 3 3)
             (cons 1 -1)
             (cons 4 -2)
             (cons -3 -3)))
               (cons (cons -3 3) (cons 4 -3)))
  (test-equal? "(cons (cons -2 3) (cons 3 3))" (cover (list (cons -2 3)
             (cons 3 3)))
               (cons (cons -2 3) (cons 3 3)))
  (test-equal? "(cons (cons -2 -2) (cons -2 -2))" (cover (list (cons -2 -2))) (cons (cons -2 -2) (cons -2 -2)))
  (test-equal? "(cons (cons -2 5) (cons 5 -2))" (cover (list (cons -2 -2) (cons 5 5))) (cons (cons -2 5) (cons 5 -2)))
  (test-equal? "(cons (cons 0 0) (cons 0 0))" (cover (list (cons 0 0) (cons 0 0))) (cons (cons 0 0) (cons 0 0)))
  (test-equal? "(cons (cons 0 1) (cons 1 0))" (cover (list (cons 0 0) (cons 1 1))) (cons (cons 0 1) (cons 1 0)))
  (test-false "'()" (cover '()))

 )
 
)
 
