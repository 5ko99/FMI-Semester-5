#lang racket
(require rackunit)
(require rackunit/gui)
(require "tree.rkt")
(test/gui
 (test-suite
  "tree? testes"
  (test-case "Valid Trees"
             (check-true(tree? "{5 * {2 * *}}"))
             )
  (test-case "UnvalidTrees"
             (check-false (tree? "{5 * {2 * *}"))
             )
  ))