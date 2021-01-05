allNatPairs = [[(x, y) | x <- [0 .. p], y <- [0 .. p], x + y == p] | p <- [0 ..]]

allNatTriplets = [(x, y, z) | p <- [0 ..], x <- [0 .. p], y <- [0 .. p], z <- [0 .. p], x + y + z == p]

pit :: [(Integer, Integer, Integer)]
pit = [(a, b, c) | (a, b, c) <- allNatTriplets, a ^ 2 + b ^ 2 == c ^ 2, a + b > c, a + c > b, b + c > a]

pairs = [(d - i, i) | d <- [0 ..], i <- [0 .. d]]

primes = sieve [2 ..]
  where
    sieve (x : xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

reverse' :: Foldable t => t a -> [a]
reverse' = foldr (\el res -> res ++ [el]) []

reverse'' :: Foldable t => t a -> [a]
reverse'' = foldl (flip (:)) []

compress [] = []
compress lst@(h : _) = (h, numHeads) : compress rest
  where
    (head, rest) = span (== h) lst
    numHeads = length head

maxRepeated lst = maximum (map snd (compress lst))