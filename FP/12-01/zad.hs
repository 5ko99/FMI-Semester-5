import Data.List (maximumBy, nub, sortBy, sortOn)
import Data.Ord (comparing)
import Test.Hpec
-- import Test.HUnit

mostFrequent :: (Eq a) => [a] -> a
mostFrequent xs = fst $ maximumBy (comparing snd) histo
  where
    histo = histogram xs

histogram :: Eq a => [a] -> [(a, Int)]
histogram xs = [(x, count x xs) | x <- uniques xs]
  where
    count x xs = length $ filter (== x) xs

uniques :: Eq a => [a] -> [a]
uniques = nub

specialSort :: (Eq a, Ord a) => [[a]] -> [[a]]
specialSort = sortOn mostFrequent

applyTo5 :: Num t1 => (t1 -> t2) -> t2
applyTo5 f = f 5
