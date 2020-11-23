#lang racket
(require rackunit)
(require rackunit/gui)
(require "tree.rkt")
(test/gui
 (test-suite
  "tree? testes"
  (test-case "Valid Trees"
             (check-true (tree? "{5 * {2 * *}}"))
             (check-true(tree? "{2**}"))
             (check-true(tree? "{2     {4     * *}               *}"))
             (check-true(tree? "*"))
             )
  
  (test-case "UnvalidTrees"
              (check-false (tree? "{5 * {2 * *}"))
              (check-false (tree? "{5 * {*}}"))
              (check-false (tree? "abcd"))
              (check-false (tree? "{2 *{3 * *} {3 * {2 *}"))
              (check-false (tree? "{2 {2 * * *} *}"))
              )
  )
 )