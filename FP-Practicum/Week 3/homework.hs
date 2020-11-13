--https://ichko.github.io/fmi-fp-2020-21/week-3/
rules :: [(Char, [Char])]
rules = [('p', "etko"), ('e', "tko"),('t',"ko"),('k',"o"),('o',"p")]
axiom = "petko"

lookup' :: Eq k => [(k, v)] -> k -> v -> v
lookup' mappings key missingValue =
  case mappings of
    [] -> missingValue
    ((k, v) : t) ->
      if k == key
        then v
        else lookup' t key missingValue


lSystem :: Eq a => [a] -> [(a, [a])] -> [[a]]
lSystem axiom rules = loop axiom
  where
    nextCharState char = lookup' rules char [char]

    getNextState state = concat [nextCharState char | char <- state]

    loop state = state : loop (getNextState state)


--quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (h:t) =
  let
    sm = quicksort $ filter (<=h) t
    bg = quicksort $ filter (>h) t
    in
      sm ++ [h] ++ bg


--collatz
collatz 1 = [1]
collatz n = n : collatz m
  where
    m = if even n then n `div` 2 else 3 * n + 1

--Newton method
type Function a = a -> a

derive :: Fractional a => a -> (a -> a) -> a -> a
derive eps f x = (f (x + eps) - f x) / eps

df :: (Function Double) -> Double -> Double
df = derive 1e-10

newton'sMethod its x0 f = loop its x0
  where
    nextStep xn = xn - (f xn / df f xn)

    loop 0 x = x
    loop its x = loop (its - 1) $ nextStep x
