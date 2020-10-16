a::Double
a = 1

b::Double
b = 10 / a

c::Integer
c=999

d :: Double
d=1.8

sum' :: Num a => a -> a -> a
sum' a b = a + b

factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' n = 
    if n==0 then 1
    else n * factorial'(n - 1)

-- 10:[1,2,3]
-- concatenation [1,2,3] ++ [4,5,6]

greet :: [Char] -> [Char]
greet name = "Hello " ++ name

-- [1,2,3] !! 0 -> 1
-- null []
-- take 2 [1,2,3,4] -> [1,2]
-- drop
-- sum,product ,concat - flatt, maximum, minimum, can compare
-- elem (elem) (list)
-- [10 .. 1000] ->builds list
{--
    can build arithmetic progressions
    [10, 12 .. 110]
    a = [1,2 .. ] - infinity list
    pred a, succ b
    cycle [1,2,3]

    let s = [2*x|x<-[0..], x^2>3]

    [x*y| x<-[1,2,3], y<-[4,5,6], x*y>3]
--}