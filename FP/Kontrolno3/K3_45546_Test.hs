{--
  СУ "Св. Климент Охридски"
  Факултет по математика и информатика
  Курс Функционално програмиране 2020/21
  Контролно 3
  2021-01-16

  Име: Петко Каменов
  ФН: 45546
  Специалност: Информатика
  Курс: Трети
  Административна група: Първа
  Начален час на контролното: 9:45
--}
module K3_45546_Test where

import K3_45546
  ( Tree (EmptyTree, Node),
    isInjective,
    treeContains,
  )
import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~:),
    (~=?),
    (~?=),
  )

--zad1 Tests
sampleTree :: Tree Char
sampleTree =
  Node
    'a'
    ( Node
        'b'
        ( Node
            'd'
            EmptyTree
            (Node 'g' EmptyTree EmptyTree)
        )
        (Node 'e' EmptyTree EmptyTree)
    )
    ( Node
        'c'
        EmptyTree
        (Node 'f' EmptyTree EmptyTree)
    )

treeTests :: Test
treeTests =
  TestList
    [ "empty string"
        ~: True ~=? treeContains sampleTree "",
      "empty Tree and empty string"
        ~: treeContains EmptyTree "" ~?= True,
      "abdg"
        ~: treeContains sampleTree "abdg" ~?= True,
      "bd"
        ~: treeContains sampleTree "bd" ~?= True,
      "abb"
        ~: treeContains sampleTree "abb" ~?= False,
      "zx"
        ~: treeContains sampleTree "zx" ~?= False,
      "af"
        ~: treeContains sampleTree "af" ~?= True,
      "ae"
        ~: treeContains sampleTree "ae" ~?= True,
      "abdg"
        ~: treeContains sampleTree "abdg" ~?= True,
      "abdg"
        ~: treeContains sampleTree "me" ~?= False,
      "one node tree"
        ~: treeContains (Node 'a' EmptyTree EmptyTree) "a" ~?= True
    ]

--zad2 Tests

functionTests :: Test
functionTests =
  TestList
    [ "x-1"
        ~: isInjective (+ (-1)) 1 20 ~?= True,
      "^2"
        ~: isInjective (^ 2) 0 10 ~?= True,
      "^2 -5:5"
        ~: isInjective (^ 2) (-5) 5 ~?= False,
      "^3"
        ~: isInjective (^ 3) 10 25 ~?= True,
      "abs in positive"
        ~: isInjective abs 1 10 ~?= True,
      "abs in -3:3"
        ~: isInjective abs (-3) 3 ~?= False,
      "mod 3"
        ~: isInjective (`mod` 3) 1 10 ~?= False,
      "mod 10 from 1 to 10"
        ~: isInjective (`mod` 10) 1 10 ~?= True,
      "(^2) . abs from -6 to 6"
        ~: isInjective ((^ 2) . abs) (-6) 6 ~?= False,
      "mod 10 from 1 to 20"
        ~: isInjective (`mod` 10) 1 20 ~?= False
    ]

main :: IO Counts
main = do
  runTestTT treeTests
  runTestTT functionTests