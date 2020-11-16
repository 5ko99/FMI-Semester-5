zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (h1 : t1) (h2 : t2) = f h1 h2 : zipWith' f t1 t2

lambdaTest = map (\x -> x ^ 2 + 3) [1 .. 30]

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (x1 - x2) * (y1 - y2)

data Student' = Student'
  { firstName :: String,
    lastName :: String,
    facultyNumber :: Int,
    bio :: String
  }
  deriving (Show)

data Vector a = Vector a a a deriving (Show, Eq)

type Frequency = Integer

getFrequency :: (Eq a) => [a] -> a -> Frequency
getFrequency list el = helper list el 0
  where
    helper [] _ res = res
    helper (h : t) el res
      | h == el = helper t el (res + 1)
      | otherwise = helper t el res

data Histogram a = Histogram [(a, Frequency)] deriving (Show, Eq)

member :: Eq t => [t] -> t -> Bool
member [] _ = False
member (h : t) el
  | h == el = True
  | otherwise = member t el

getUniques :: Eq a => [a] -> [a]
getUniques l = helper l []
  where
    helper [] res = res
    helper (h : t) res
      | member t h = helper t res
      | otherwise = helper t (h : res)

histogram l = helper l (getUniques l) []
  where
    helper _ [] res = res
    helper list (h : t) res = helper list t ((h, freq) : res)
      where
        freq = getFrequency list h