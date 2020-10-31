type Vector a = (a,a,a)

length' [x,y,z] = sqrt(x^2 + y^2 +z^2)

main = do
    putStr("Hello!")
    return ()

map' :: (t -> a) -> [t] -> [a]
map' _ [] = []
map' f (h:t) = f h : map' f t

square x = x * x

derive :: Fractional a => a -> (a -> a) -> a -> a
derive eps f x = (f (x + eps) - f x) / eps

df :: (Double -> Double) -> Double -> Double
df = derive 1e-10

--integral
integrate :: (Num a, Enum a) => a -> (a, a) -> (a -> a) -> a
integrate eps (a, b) f = 
    sum . map (* eps) . map f $ [a, a + eps .. b]

(~∫) :: (Double, Double) -> (Double -> Double) -> Double
(~∫) = integrate 1e-4

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys



