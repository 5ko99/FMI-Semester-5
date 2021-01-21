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
module K3_45546 where

import Data.List (nub)

-- zad 1
data Tree a
  = EmptyTree
  | Node
      { value :: a,
        left :: Tree a,
        right :: Tree a
      }
  deriving (Show, Read)

treeContains :: Tree Char -> String -> Bool
treeContains _ [] = True
treeContains EmptyTree _ = False
treeContains t@(Node v l r) str@(x : xs)
  | v == x = treeContains l xs || treeContains r xs
  | otherwise = treeContains l str || treeContains r str

--zad2

isInjective :: Integral t => (t -> t) -> t -> t -> Bool
isInjective f a b = values == nub values
  where
    lst = [a .. b]
    values = [f i | i <- lst]
