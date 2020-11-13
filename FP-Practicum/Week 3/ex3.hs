test = map even [5,-3,-6,7,-3,2,-19,24]

derive :: Fractional a => a -> (a -> a) -> a -> a
derive eps f x = (f (x + eps) - f x) / eps

df :: (Double -> Double) -> Double -> Double
df = derive 1e-10