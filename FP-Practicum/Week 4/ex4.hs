zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (h1 : t1) (h2 : t2) = f h1 h2 : zipWith' f t1 t2